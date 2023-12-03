(load-file "trebuchet.el")

(declare-function trebuchet "trebuchet.el" (file-path))

(ert-deftest trebuchet-test ()
  (should (equal (trebuchet "../fixture.txt") 142)))

(ert-deftest trebuchet-input()
  (should (equal (trebuchet "../input.txt") 55538)))
