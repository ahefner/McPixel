(asdf:defsystem :mcpixel
    :name "McPixel"
    :description "McCLIM pixel art editor"
    :version "1"
    :author "Andy Hefner <ahefner@gmail.com>"
    :license "MIT-style license"
    :depends-on (:mcclim :alexandria :skippy)
    :serial t
    :components ((:file "mcpixel")))
