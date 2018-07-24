(ns witan.datasets
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.core.matrix :as cm]
            [clojure.core.reducers :as r]
            [incanter.core :as i]
            [incanter.stats :as st]))

(defn property-holds? [x p msg]
  (if (p x)
    x
    (throw (Exception. msg))))

(defn safe-divide
  [[d dd]]
  (if (zero? dd)
    0
    (/ d dd)))

(defn juxt-r
  [& fns]
  (fn
    ([]
     (map #(%) fns))
    ([x]
     (map #(%1 %2) fns x))
    ([a x]
     (map #(%1 %2 %3) fns a x))))

(def rollup-fns
  {:max [identity max identity]
   :min [identity min identity]
   :sum [identity + identity]
   :count [(constantly 1) + identity]
   :mean [(juxt identity (constantly 1))
          (juxt-r + +)
          safe-divide]})

(defn rollup
  "Returns a dataset that uses the given summary function (or function
  identifier keyword) to rollup the given column based on a set of
  group-by columns. You can provide a keyword identifier of a set of
  built-in functions including:
  :max -- the maximum value of the data in each group
  :min -- the minimum value of the data in each group
  :sum -- the sum of the data in each group
  :count -- the number of elements in each group
  :mean -- the mean of the data in each group
  or you can supply your own triple of [transformer, accumulator, finalizer]"
  [data summary-fun col-name group-by]
  (let [sub-data (ds/select-columns data (conj group-by col-name))
        column-dexs (into {} (map-indexed #(vector %2 %1) (:column-names sub-data)))
        grouper (fn [values]
                  (mapv (fn [col]
                          (nth values (column-dexs col)))
                        group-by))
        [xf accumulate finalizer] (if (keyword? summary-fun)
                                    (rollup-fns summary-fun)
                                    summary-fun)]
    (->> sub-data
         cm/rows
         (r/map (fn transform
                  ([values]
                   [(grouper values)
                    (xf (nth values (column-dexs col-name)))])))
         (r/fold
          (fn combiner
            ([] {})
            ([l r]
             (merge-with accumulate
                         l r)))
          (fn reducer
            ([] {})
            ([a [group value]]
             (update a
                     group
                     #(if %
                        (accumulate % value)
                        (accumulate value))))))
         (r/map (fn [[g v]]
                  (conj g (finalizer v))))
         (into [])
         (ds/dataset (conj group-by col-name)))))

(defn add-derived-column
  "Adds or replaces a column in the dataset, with values (apply derive-fn (select-columns src-col-name (rows dataset))).
   If derived-col-name already exists within the dataset, the column is replaced with the new values."
  [dataset derived-col-name src-col-names derive-fn]
  (let [derived-col-name-present ((set (ds/column-names dataset)) derived-col-name)]
    ((if derived-col-name-present ds/replace-column ds/add-column)
     dataset derived-col-name
     (apply (partial map derive-fn)
            (map #(ds/column dataset %) src-col-names)))))

(defn row-count
  "This should be added to core.matrix, and probably to the dataset protocol"
  [dataset]
  (first (:shape dataset)))

(defn build-index [indexer inverse-indexer dataset]
  (->> dataset
       cm/rows
       (r/fold
        (fn combiner
          ([] {})
          ([l r]
           (merge l r)))
        (fn reducer
          ([] {})
          ([a row]
           (assoc a
                  (indexer row)
                  (inverse-indexer row)))))))

(defn column-values-fn
  [dataset columns]
  (let [col-indexes (map (partial ds/column-index dataset) columns)]
    (fn [row]
      (map (partial nth row) col-indexes))))

(defn join-
  "Right joins the two datasets by the values found in columns, where the left side of the join is target.
  Implementation assumes the cost of converting the left dataset to rows and then using fold to join right, will be
  justified by the size of the data. Potential improve could be to detect the data size and if small perform the join by
  creating new columns for the dataset, rather than growing the rows."
  [target src [t-columns s-columns] {:keys [empty-cell] :or {:empty-cell nil}}]
  (let [src-indexer (column-values-fn src s-columns)
        unindexed-cols (remove (set s-columns) (ds/column-names src))
        inverse-indexer (column-values-fn src unindexed-cols)
        dex (build-index src-indexer inverse-indexer src)
        t-indexer (column-values-fn target t-columns)
        unmatched-index (repeat (count unindexed-cols) empty-cell)]
    (->> target
         cm/rows
         (r/fold
          (fn combiner
            ([] [])
            ([l r]
             (concat l r)))
          (fn reducer
            ([] [])
            ([a row]
             (conj a
                   (concat row
                           (get dex (t-indexer row) unmatched-index))))))
         (ds/dataset (concat (ds/column-names target)
                             unindexed-cols)))))

(defn left-join
  [left right columns & options]
  (join- left right
        (if (vector? (first columns)) columns (repeat 2 columns))
        (apply hash-map options)))

(defn right-join
  [left right columns & options]
  (join- right left
        (if (vector? (first columns)) columns (repeat 2 columns))
        (apply hash-map options)))

(defn join
  "Performs a right-join"
  [left right columns & options]
  (apply (partial right-join left right columns) options))

(defn filter-dataset
  "Filters the given dataset to rows where filter-fn returns truthy when called with the values in filter-columns"
  [dataset filter-columns filter-fn]
  (let [filter-indexes (map #(ds/column-index dataset %) filter-columns)
        filter-on (fn [column-values]
                    (map #(get column-values %) filter-indexes))
        empty-data (mapv (constantly []) (ds/column-names dataset))]
    (->> (:columns dataset)
         (apply (partial mapv vector))
         (r/fold
          (fn combiner
            ([] empty-data)
            ([l r]
             (mapv concat l r)))
          (fn reducer
            ([] empty-data)
            ([a column-values]
             (if (apply filter-fn (filter-on column-values))
               (mapv #(conj %1 %2) a column-values)
               a))))
         (zipmap (ds/column-names dataset))
         ds/dataset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers for Incanter functions to use on datasets (from incanter.core)
;; Used in `witan.models.demography` and `witan.models.household`:
(defn select-from-ds
  "Takes in a dataset and where clauses as a map
   like `{:col1 {:gte 4} :col2 {:gt 5}}` or as a predicate function.
   Returns a result dataset from selecting the where clauses. It uses
   :eq -> `=`
   :gt -> `>` or :gte -> `>=`
   :lt -> `<` or :lte -> `<=`
   :in or :nin for element in r not in a set."
  [from-dataset where-clauses]
  (i/query-dataset from-dataset where-clauses))

(defn subset-ds
  "Takes in dataset and a series of options like `:rows 1 :cols :col1`.
   returns a subset of the dataset according the options specified and as
   a single element or as a collection of elements.
   The rows are selected by indexes and the columns by name or indexes.
   Here are the options available:
   :rows (default true)
      returns all rows by default, can pass a row index or sequence of row indices
   :cols (default true)
      returns all columns by default, can pass a column index or sequence of column indices
   :except-rows (default nil) can pass a row index or sequence of row indices to exclude
   :except-cols (default nil) can pass a column index or sequence of column indices to exclude
   :filter-fn (default nil)
      a function can be provided to filter the rows of the matrix"
  [initial-dataset & options]
  (apply i/sel initial-dataset options))

(defn group-ds
  "Takes in a dataset and a column name or a vector of column names.
   Returns a map with a map of grouped-by element as a key and grouping
   result as a value."
  [initial-dataset column]
  (i/$group-by column initial-dataset))

(defn linear-model
  "Takes in two arguments y (dependent variable) a vector of values and x
   (independent variables) a vector or matrix of values, plus an option.
   The option :intercept (default true) indicates weather an intercept
   term should be included.
   Returns the results of performing a linear regression of y on x.
   Here's the content of the map of results:
   :design-matrix -- a matrix containing the independent variables, and an intercept columns
   :coefs -- the regression coefficients
   :t-tests -- t-test values of coefficients
   :t-probs -- p-values for t-test values of coefficients
   :coefs-ci -- 95% percentile confidence interval
   :fitted -- the predicted values of y
   :residuals -- the residuals of each observation
   :std-errors -- the standard errors of the coeffients
   :sse -- the sum of squared errors, also called the residual sum of squares
   :ssr -- the regression sum of squares, also called the explained sum of squares
   :sst -- the total sum of squares (proportional to the sample variance)
   :r-square -- coefficient of determination"
  [y x & option]
  (if (nil? option)
    (st/linear-model y x)
    (let [option-key (property-holds? (first option) #(= % :intercept)
                                            (str (first option) " should be `:intercept`." ))
          option-val (property-holds? (second option) number?
                                            (str (second option) " should be a number." ))]
      (st/linear-model y x option-key option-val))))
