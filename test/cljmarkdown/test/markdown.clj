(ns cljmarkdown.test.markdown
  (:use [cljmarkdown.markdown])
  (:use [clojure.test]))

(deftest md-strong
  (is (= "<strong>markdown</strong>" (md2html "**markdown**")))
  (is (= "markdown" (md2html "markdown")))
  (is (= "* * markdown**" (md2html "* * markdown**")))
  (is (= "**markdown * *" (md2html "**markdown * *")))
  (is (= "<strong>**</strong>" (md2html "******")))
  (is (= "<strong>*</strong>" (md2html "*****")))
  (is (= "<strong>*strong</strong>" (md2html "***strong**"))))

(deftest md-em
  (is (= "<em>markdown</em>" (md2html "*markdown*")))
  (is (= "<em>markdown*</em>" (md2html "*markdown**")))
  (is (= "markdown" (md2html "markdown")))
  (is (= "* markdown*" (md2html "* markdown*")))
  (is (= "*markdown *" (md2html "*markdown *")))
  (is (= "<em>*</em>" (md2html "***")))
  (is (= "<em>strong*</em>" (md2html "*strong**"))))

(deftest md-strong-em
  (is (= "<strong><em>*</em></strong>" (md2html "*******")))
  (is (= "<strong>*</strong>" (md2html "*****")))
  (is (= "<strong>hello<em>world</em></strong>" (md2html "**hello*world***")))
  (is (= "<em>world**</em>" (md2html "*world***")))
  (is (= "* hello * world*" (md2html "* hello * world*"))))

(deftest md-link
  (is (= "<a href=\"http://google.com\">Google</a>" (md2html "[Google](http://google.com)")))
  (is (= "<a href=\"\">Google</a>" (md2html "[Google]()")))
  (is (= "<a href=\"http://google.com\">Google</a>" (md2html "[Google](http://google.com)")))
  (is (= "This is not a valid [link] " (md2html "This is not a valid [link] ")))
  (is (= "This is not a valid [link] ()" (md2html "This is not a valid [link] ()"))))

(deftest md-line
  (is (= "This is really <strong>fun</strong>. And you can find me at <a href=\"http://github.com/kevin4byte\"><em>Github</em></a>"
         (md2html "This is really **fun**. And you can find me at [*Github*](http://github.com/kevin4byte)"))))

(deftest md-escape
  (is (= "**" (md2html "\\**")))
  (is (= "**" (md2html "*\\*")))
  (is (= "**\\" (md2html "**\\")))
  (is (= "*hello*" (md2html "\\*hello*")))
  (is (= "<em>*hello*</em>" (md2html "**hello\\**")))
  (is (= "*literal asterisks*" (md2html "\\*literal asterisks\\*")))
  (is (= "\\" (md2html "\\\\")) "escape \\")
  (is (= "**two asterisks**" (md2html "\\*\\*two asterisks\\*\\*")))
  (is (= "<strong>*hello*</strong>" (md2html "**\\*hello\\***")))
  (is (= "[Google](http://google.com)" (md2html "\\[Google](http://google.com)")))
  (is (= "[Google](http://google.com)" (md2html "[Google\\](http://google.com)")))
  (is (= "[Google](http://google.com)" (md2html "[Google]\\(http://google.com)")))
  (is (= "[Google](http://google.com)" (md2html "[Google](http://google.com\\)")))
  (is (= "<a href=\"http://google.com\">[Google]</a>" (md2html "[[Google\\]](http://google.com)")))
  (is (= "\\ ` + - _ * ! . { } ( ) [ ] #") (md2html "\\\\ \\` \\+ \\- \\_ \\* \\! \\. \\{ \\} \\( \\) \\[ \\] \\#")))
