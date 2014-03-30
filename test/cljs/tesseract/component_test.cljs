(ns tesseract.component-test
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var use-fixtures)])
  (:require [cemerick.cljs.test :as t]
            [tesseract.core :as core :refer-macros [defcomponent]]
            [tesseract.component :as component]
            [tesseract.cursor]
            [tesseract.dom :as dom]))

(use-fixtures :each
              (fn [f]
                "Clean up any components still mounted after all tests"
                (f)
                (tesseract.core/unmount-all!)))

(defcomponent OrderedList
  (will-build! [this next-component])
  (did-build! [this prev-component root-node])
  (will-mount! [this] this)
  (did-mount! [this root-node])
  (render [component]
          (apply dom/ol
                 {:class :test-component}
                 (for [child (-> component :attrs :children)]
                   (dom/li {} child)))))

(deftest defines-convenience-constructor
  (is (ifn? OrderedList)))

(deftest satisfies-protocols
  (let [c (OrderedList {})]
    (is (satisfies? component/IComponent c))
    (is (satisfies? component/IWillBuild c))
    (is (satisfies? component/IDidBuild c))
    (is (satisfies? component/IWillMount c))
    (is (satisfies? component/IDidMount c))))

(deftest has-children-attr
  (let [c (OrderedList {} "first" "second")]
    (is (= ["first" "second"] (-> c :attrs :children)))))

(deftest test-render
  (let [c (OrderedList {} "first" "second")]
    (is (= (dom/ol {:class :test-component}
                   (dom/li {} "first")
                   (dom/li {} "second"))
           (component/render c)))))

(deftest test-toString
  (let [c (OrderedList {:on-click (fn [_ _])} "first" "second")]
    (is (= "<ol class=\"test-component\"><li>first</li><li>second</li></ol>"
           (str c)))))

(deftest test-attach!
  (let [c (OrderedList {} "first" "second")]
    (core/attach! c js/document.body)
    (is (= js/document.body.children.length 1))
    (is (= js/document.body.firstChild.nodeName "OL"))
    (is (= js/document.body.firstChild.children.length 2))
    (is (= "first" (.-textContent (aget js/document.body.firstChild.children 0))))
    (is (= "second" (.-textContent (aget js/document.body.firstChild.children 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcomponent Comment
  (render [{:keys [attrs]}]
    (dom/div {:class :comment}
             (dom/h2 {:class :comment-author} (:author attrs))
             (:children attrs))))

(defcomponent CommentList
  (render [component]
    (dom/div {:class :comment-list}
             (Comment {:author "Logan Linn"} "This is one comment")
             (Comment {:author "Scott Rabin"} "This is *another* comment"))))

(deftest test-comment-list
  (let [comment-list (CommentList {})
        out (component/render comment-list)]
    (is (= :div (:tag out)))
    (is (= :comment-list (-> out :attrs :class)))
    (is (= 2 (count (:children out))))
    (is (= "<div class=\"comment-list\"><div class=\"comment\"><h2 class=\"comment-author\">Logan Linn</h2>This is one comment</div><div class=\"comment\"><h2 class=\"comment-author\">Scott Rabin</h2>This is *another* comment</div></div>"
           (str out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-component-cursor
  (let [comments (CommentList {:version 1})
        comments' (update-in comments [:attrs :version] inc)
        cursor (tesseract.cursor/->cursor :root-id)]
    (testing "tesseract.IComponent/-build! associates cursor"
      (let [built-comments (component/-build! comments' comments cursor)
            num-gchildren (-> built-comments
                              (get-in [:children 0 :children])
                              (count))]
        (is (= cursor (tesseract.cursor/get-cursor built-comments)))
        (testing "child cursors"
          (is (= (conj cursor 0)
                 (-> built-comments
                     (get-in [:children 0])
                     (tesseract.cursor/get-cursor))))
          (dotimes [n num-gchildren]
            (is (= (conj cursor 0 n)
                   (-> built-comments
                       (get-in [:children 0 :children n])
                       (tesseract.cursor/get-cursor))))))))
    (testing "tesseract.IComponent/-unmount! dissociates cursor"
      (let [will-unmount-calls (atom 0)
            built-comments (-> (component/-build! comments' comments cursor)
                               (specify! component/IWillUnmount
                                         (-will-unmount! [this]
                                                         (swap! will-unmount-calls inc))))
            num-gchildren (-> built-comments
                              (get-in [:children 0 :children])
                              (count))]
        (component/unmount! built-comments)
        (testing "tesseract.IWillMount/-will-unmount! invoked"
          (is (= 1 @will-unmount-calls)))
        (testing "dissociates cursor"
          (is (nil? (tesseract.cursor/get-cursor built-comments))))
        (testing "dissociates child cursors"
          (is (nil? (-> built-comments
                        (get-in [:children 0])
                        (tesseract.cursor/get-cursor))))
          (dotimes [n num-gchildren]
            (is (nil? (-> built-comments
                          (get-in [:children 0 :children n])
                          (tesseract.cursor/get-cursor))))))))))
