(ns cljmarkdown.test.markdown
  (:use [cljmarkdown.markdown])
  (:use [clojure.test]))

(defmacro asserts
  [tname cases]
  `(deftest ~(symbol tname)
            ~@(for [[excpted given] cases]
              `(is (= ~excpted (md2html ~given))))))

(asserts "md-strong" [["<strong>markdown</strong>" "**markdown**"]
                       ["markdown" "markdown"]
                       ["* * markdown**" "* * markdown**"]
                       ["**markdown * *" "**markdown * *"]
                       ["<strong>**</strong>" "******"]
                       ["<strong>*</strong>" "*****"]
                       ["<strong>*strong</strong>" "***strong**"]
                       ])

(asserts "md-em" [["<em>markdown</em>" "*markdown*"]
                  ["<em>markdown*</em>" "*markdown**"]
                  ["markdown" "markdown"]
                  ["* markdown*" "* markdown*"]
                  ["*markdown *" "*markdown *"]
                  ["<em>*</em>" "***"]
                  ["<em>strong*</em>" "*strong**"]
                  ])

(asserts "md-strong-em" [["<strong><em>*</em></strong>" "*******"]
                         ["<strong>*</strong>" "*****"]
                         ["<strong>hello<em>world</em></strong>" "**hello*world***"]
                         ["<em>world**</em>" "*world***"]
                         ["* hello * world*" "* hello * world*"]
                         ])

(asserts "mk-link" [["<a href=\"http://google.com\">Google</a>" "[Google](http://google.com)"]
                    ["<a href=\"\">Google</a>" "[Google]()"]
                    ["<a href=\"http://google.com\">Google</a>" "[Google](http://google.com)"]
                    ["This is not a valid [link] " "This is not a valid [link] "]
                    ["This is not a valid [link] ()" "This is not a valid [link] ()"]
                    ])

(asserts "md-line" [["This is really <strong>fun</strong>. And you can find me at <a href=\"http://github.com/kevin4byte\"><em>Github</em></a>"
         "This is really **fun**. And you can find me at [*Github*](http://github.com/kevin4byte)"]])

(asserts "md-escape" [["**" "\\**"]
                      ["**" "*\\*"]
                      ["**\\" "**\\"]
                      ["*hello*" "\\*hello*"]
                      ["<em>*hello*</em>" "**hello\\**"]
                      ["*literal asterisks*" "\\*literal asterisks\\*"]
                      ["\\" "\\\\" "escape \\"]
                      ["**two asterisks**" "\\*\\*two asterisks\\*\\*"]
                      ["<strong>*hello*</strong>" "**\\*hello\\***"]
                      ["[Google](http://google.com)" "\\[Google](http://google.com)"]
                      ["[Google](http://google.com)" "[Google\\](http://google.com)"]
                      ["[Google](http://google.com)" "[Google]\\(http://google.com)"]
                      ["[Google](http://google.com)" "[Google](http://google.com\\)"]
                      ["<a href=\"http://google.com\">[Google]</a>" "[[Google\\]](http://google.com)"]
                      ["\\ ` + - _ * ! . { } ( ) [ ] #" "\\\\ \\` \\+ \\- \\_ \\* \\! \\. \\{ \\} \\( \\) \\[ \\] \\#"]
                      ])
