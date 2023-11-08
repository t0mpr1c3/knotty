#lang typed/racket

#| commented out to avoid recompilation during CI

(require knotty)

(define
  leaf
  (pattern
    #:name "Embossed Leaf"
    #:url  "https://stitch-maps.com/patterns/display/11983/"
    ((row( 1) k3 k2tog (repeat p2 yo k1 yo p2 ssk k5 k2tog) p2 yo k1)
    ((row( 3) k2 k2tog (repeat p2 k1 yo k1 yo k1 p2 ssk k3 k2tog) p2 k1 yo k1)
    ((row( 5) k1 k2tog (repeat p2 k2 yo k1 yo k2 p2 ssk k1 k2tog) p2 k2 yo k1)
    ((row( 7) k2tog (repeat p2 k3 yo k1 yo k3 p2 cdd) p2 k3 yo k1)
    ((row( 9) k1 yo (repeat p2 ssk k5 k2tog p2 yo k1 yo) p2 ssk k3)
    ((row(11) k1 yo k1 (repeat p2 ssk k3 k2tog p2 k1 yo k1 yo k1) p2 ssk k2)
    ((row(13) k1 yo k2 (repeat p2 ssk k1 k2tog p2 k2 yo k1 yo k2) p2 ssk k1)
    ((row(15) k1 yo k3 (repeat p2 cdd p2 k3 yo k1 yo k3) p2 ssk)
    ((rows(seq(2 16 2)) p22)

(export-xml
  leaf
  "leaf.xml")

|#
;; application of XSL stylesheet
;; saxonb-xslt -s:leaf.xml -xsl:../xml/knitscheme-chart.xsl -o:leaf-chart.html
