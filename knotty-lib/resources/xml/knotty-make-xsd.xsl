<!--
    Knotty, a domain specific language for knitting patterns.
    Copyright (C) 2021-3 Tom Price

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-->
<!--
    This XSLT 2.0 stylesheet uses stitch information in `knotty-stitch.xml`
    to make the XML schema `knotty.xsd`.
-->

<xsl:stylesheet version="2.0"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:f="my:f"
  exclude-result-prefixes="f xs xsl">

  <xsl:output omit-xml-declaration="yes" indent="yes"/>

  <xsl:template match="stitches">
    <xsl:comment>
  Knotty, a domain specific language for knitting patterns.
  Copyright (C) 2021-3 Tom Price

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see &lt;https://www.gnu.org/licenses/&gt;.
</xsl:comment>
    <xsl:text>&#xa;</xsl:text>
    <xsl:comment>
  This file defines the schema for Knotty XML files.

  Create the schema using Saxon or similar XSLT software:

      java -jar saxon-he-11.5.jar -s:knotty-stitches.xml -xsl:knotty-make-xsd.xsl -o:knotty.xsd

  Knotty XML files can be validated using `xmllint` from `libxml`:

      xmllint ‑-schema knotty.xsd example.xml
</xsl:comment>
    <xsl:text>&#xa;</xsl:text>
    <xs:schema
      xmlns:xs="http://www.w3.org/2001/XMLSchema"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment>
  `patterntype` defines a stitch pattern and does not contain garment or
  shape information beyond what is need to encode a colorwork design or
  lace motif.
</xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="pattern-type">
        <xs:sequence>
          <xs:element name="name"                type="xs:string"                minOccurs="0"/>
          <xs:element name="url"                 type="xs:string"                minOccurs="0" default=""/>
          <xs:element name="attribution"         type="attribution-type"         minOccurs="0"/>
          <xs:element name="keywords"            type="keywords-type"            minOccurs="0"/>
          <xs:element name="options"             type="options-type"             minOccurs="0"/>
          <xs:element name="dimensions"          type="dimensions-type"          minOccurs="1"/> <!-- required -->
          <xs:element name="yarns"               type="yarns-type"               minOccurs="0"/>
          <xs:element name="stitch-instructions" type="stitch-instructions-type" minOccurs="0"/>
          <xs:element name="row-data"            type="row-data-type"            minOccurs="0"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> optional attribution </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="attribution-type">
        <xs:sequence>
          <xs:element name="author" type="author-type" minOccurs="0"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for author information </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="author-type">
        <xs:sequence>
          <xs:element name="name" type="xs:string" minOccurs="1"/>
          <xs:element name="url"  type="xs:string" minOccurs="0" default="''"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for optional keywords </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="keywords-type">
        <xs:sequence>
          <xs:element name="keyword" type="keyword-type" minOccurs="0" maxOccurs="32"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for individual keyword </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="keyword-type">
        <xs:restriction base="xs:token">
          <xs:maxLength value="32"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for pattern options </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="options-type">
        <xs:sequence>
          <xs:element name="technique" type="technique-type" minOccurs="0"/>
          <xs:element name="form"      type="form-type"      minOccurs="0"/>
          <xs:element name="face"      type="face-type"      minOccurs="0"/>
          <xs:element name="side"      type="side-type"      minOccurs="0"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> whether the piece is knit by "hand" or using a "machine" </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="technique-type">
        <xs:restriction base="xs:token">
          <xs:enumeration value="hand"/>
          <xs:enumeration value="machine"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> whether the piece is worked "flat" or in the round ("circular") </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="form-type">
        <xs:restriction base="xs:token">
          <xs:enumeration value="flat"/>
          <xs:enumeration value="circular"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> whether the first row is worked on the right side ("rs") or wrong
      side ("ws") of the piece </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="face-type">
        <xs:restriction base="xs:token">
          <xs:enumeration value="rs"/>
          <xs:enumeration value="ws"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> whether the first row is worked from the LHS or from the RHS </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="side-type">
        <xs:restriction base="xs:token">
          <xs:enumeration value="right"/>
          <xs:enumeration value="left"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> required type for pattern dimensions </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="dimensions-type">
        <xs:sequence>
          <xs:element name="row-count"        type="natural"          minOccurs="1"/>
          <xs:element name="cast-on-count"    type="natural"          minOccurs="1"/>
          <xs:element name="cast-on-repeat"   type="natural"          minOccurs="1"/>
          <xs:element name="row-repeat-first" type="positive-integer" minOccurs="0"/>
          <xs:element name="row-repeat-last"  type="positive-integer" minOccurs="0"/>
          <xs:element name="gauge"            type="gauge-type"       minOccurs="0"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for optional gauge specification </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="gauge-type">
        <xs:sequence>
          <xs:element name="stitch-count"       type="positive-integer"/>
          <xs:element name="stitch-measurement" type="positive-integer"/>
          <xs:element name="row-count"          type="positive-integer"/>
          <xs:element name="row-measurement"    type="positive-integer"/>
          <xs:element name="measurement-unit"   type="measurement-unit-type"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for measurement unit </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="measurement-unit-type">
        <xs:union>
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="cm"/>
              <xs:enumeration value="inch"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:union>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for optional yarn specification </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="yarns-type">
        <xs:sequence>
          <xs:element name="yarn" type="yarn-type" minOccurs="0" maxOccurs="256"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for individual yarn </xsl:comment>
      <xsl:comment> default color is white </xsl:comment>
      <xsl:comment> FIXME could use a more structured type for yarn weight </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="yarn-type">
        <xs:sequence>
          <xs:element name="number" type="byte"                           default="0"/>
          <xs:element name="color"  type="color-type"                     default="FFFFFF"/>
          <xs:element name="name"   type="xs:string"        minOccurs="0" default="''"/>
          <xs:element name="weight" type="yarn-weight-type" minOccurs="0" default="''"/>
          <xs:element name="fiber"  type="xs:string"        minOccurs="0" default="''"/>
          <xs:element name="brand"  type="xs:string"        minOccurs="0" default="''"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for yarn weight </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="yarn-weight-type">
        <xs:union memberTypes="empty">
          <xs:simpleType>
            <xs:restriction base="xs:integer">
              <xs:minInclusive value="0"/>
              <xs:maxInclusive value="7"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:union>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> default yarn number of 0 = MC </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="byte">
        <xs:restriction base="xs:integer">
          <xs:minInclusive value="0"/>
          <xs:maxInclusive value="255"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> color encoded as 24-bit RGB, R is most significant byte </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="color-type">
        <xs:restriction base="xs:hexBinary">
          <xs:maxLength value="6" fixed="true"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> required row data </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="row-data-type">
        <xs:sequence>
          <xs:element name="rows" type="rows-type" maxOccurs="unbounded"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> stitch and yarn information matching one or more row numbers </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="rows-type">
        <xs:sequence>
          <xs:element name="row-number"   type="positive-integer" maxOccurs="unbounded"/>
          <xs:element name="memo"         type="xs:string"        minOccurs="0" default=""/>
          <xs:element name="default-yarn" type="yarn-number-type" minOccurs="0" default="0"/>
          <xs:element name="short-row"    type="boolean"          minOccurs="0" default="0"/>
          <xs:element name="stitches"     type="stitches-type"    minOccurs="1"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for positive integer </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="positive-integer">
        <xs:restriction base="xs:integer">
          <xs:minInclusive value="1"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for yarn number </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="yarn-number-type">
        <xs:union memberTypes="byte empty"/>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> 0 = false, 1 = true </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="boolean">
        <xs:restriction base="xs:integer">
          <xs:minInclusive value="0"/>
          <xs:maxInclusive value="1"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for stitches </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="stitches-type">
        <xs:sequence>
          <xs:group ref="stitch-element-group" maxOccurs="unbounded"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> most general element type for encoding stitch information </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:group name="stitch-element-group">
        <xs:choice>
          <xs:element name="run" type="stitch-run-type" maxOccurs="unbounded"/>
          <xs:element name="seq" type="stitch-seq-type" maxOccurs="unbounded"/>
        </xs:choice>
      </xs:group>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> describes a sequence of one or more stitches of the same kind and
      using the same yarn </xsl:comment>
      <xsl:comment> absent count or count = 0 means a variable repeat </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="stitch-run-type">
        <xs:sequence>
          <xs:element name="count"  type="natural" minOccurs="0"/>
          <xs:element name="stitch" type="stitch-id-type"/>
          <xs:element name="yarn"   type="yarn-number-type"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> describes repeated element of more than one type or color of stitch </xsl:comment>
      <xsl:comment> absent count or count = 0 means a variable repeat </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="stitch-seq-type">
        <xs:sequence>
          <xs:element name="count"    type="natural"       minOccurs="0"/>
          <xs:element name="stitches" type="stitches-type" maxOccurs="unbounded"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for non-negative integers </xsl:comment>
      <xs:simpleType name="natural">
        <xs:restriction base="xs:integer">
          <xs:minInclusive value="0"/>
        </xs:restriction>
      </xs:simpleType>

      <xsl:comment> stitch identifier </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="stitch-id-type">
        <xs:union memberTypes="stitch-id-enum-type stitch-id-string-type"/>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> list of stitches </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="stitch-id-enum-type">
        <xs:restriction base="xs:token">
          <xsl:apply-templates select="//rs-symbol">
            <xsl:sort select="."/>
          </xsl:apply-templates>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> other stitches can be identified using a short alphanumeric string </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="stitch-id-string-type">
        <xs:restriction base="xs:token">
          <xs:pattern value="[A-Za-z0-9]{1,10}"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> optional instructions for how to knit each stitch </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="stitch-instructions-type">
        <xs:sequence>
          <xs:element name="stitch-instruction" type="stitch-instruction-type" minOccurs="0" maxOccurs="unbounded"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for stitch instruction </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:complexType name="stitch-instruction-type">
        <xs:sequence>
          <xs:element name="stitch" type="stitch-id-type"/>
          <xs:element name="text"   type="xs:string"/>
        </xs:sequence>
      </xs:complexType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> empty type is equivalent to boolean false </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:simpleType name="empty">
        <xs:restriction base="xs:string">
          <xs:enumeration value="''"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:text>&#xa;</xsl:text>

      <xsl:comment> type for stitch pattern </xsl:comment>
      <xsl:text>&#xa;</xsl:text>
      <xs:element name="pattern" type="pattern-type"/>
      <xsl:text>&#xa;</xsl:text>

    </xs:schema>
  </xsl:template>

  <xsl:template match="rs-symbol">
    <xsl:element name="xs:enumeration">
      <xsl:attribute name="value">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>
