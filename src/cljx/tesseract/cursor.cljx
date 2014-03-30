(ns tesseract.cursor)

(def ->cursor vector)

(defn- get-cursor*
  [component]
  (let [cursor (-> component meta ::cursor)]
    (assert cursor (str "Failed to retrieve cursor from component: " component))
    cursor))

(defn assoc-cursor
  [component cursor]
  (vary-meta component assoc ::cursor (atom cursor)))

(defn clear-cursor!
  [component]
  (reset! (get-cursor* component) nil))

(defn get-cursor
  [component]
  @(get-cursor* component))

(defn root-id
  "Returns root id of cursor" ;; TODO Subject to protocol
  [cursor]
  (first cursor))

(defn path
  "Returns path of cursor or nil if cursor is for a root-component"
  [cursor]
  (rest cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cursor->keys
  "Returns nil or a sequence of keys to locate component at cursor's path from
  the root-component. Returns nil when cursor is for a root-component"
  [cursor]
  (when-let [path (next cursor)]
   (cons :children (interpose :children path))))

(defn get-child
  "Returns child component at cursor's position, or component if cursor is for
  a root-component"
  [component cursor]
  (get-in component (cursor->keys cursor)))

(defn assoc-child
  "Returns component after associating child at cursor path. If cursor is for a
  root-component, child is returned."
  [component cursor child]
  (if-let [ks (cursor->keys cursor)]
    (assoc-in component ks child)
    child))

(defn update-child
  "Returns component after updating a child component located at cursor path by
  (apply f child-component args). If cursor is for a root component, f and args
  are applied to passed component."
  [component cursor f & args]
  (if-let [ks (cursor->keys cursor)]
    (apply update-in component ks f args)
    (apply f component args)))
