# Erlsom #

- [Introduction](#introduction)
  - [Example XML document](#example)
- [SAX Mode](#sax)
  - [SAX Events](#sax_events)
- [Simple DOM Mode](#DOM)
- [Data Binder Mode](#binder)
- [Installation](#installation)
- [Examples](#examples)
- [Character encoding](#encoding)
- [Creation of atoms](#atoms)
- [Limitations](#limitations)
  - [XML Schema elements](#elements)
  - [XML Schema Attributes](#attributes)
- [Reference](doc/reference.md)

## <a name="introduction">Introduction</a> ##
Erlsom is an Erlang library to parse (and generate) XML documents.

Erlsom can be used in a couple of very different modes:

- As a [SAX parser](#sax). This is a [more or less standardized model](
  http://www.saxproject.org/apidoc/org/xml/sax/ContentHandler.html) for
  parsing XML. Every time the parser has processed a meaningful part of the
  XML document (such as a start tag), it will tell your application about
  this. The application can process this information (potentially in
  parallel) while the parser continues to parse the rest of the document.
  The SAX parser will allow you to efficiently parse XML documents of
  arbitrary size, but it may take some time to get used to it. If you
  invest some effort, you may find that it fits very well with the Erlang
  programming model (personally I have always been very happy about my
  choice to use a SAX parser as the basis for the rest of Erlsom).

- As a simple sort of [DOM parser](#DOM). Erlsom can translate your XML to
  the ‘simple form’ that is used by Xmerl. This is a form that is easy to
  understand, but you have to search your way through the output to get to
  the information that you need.

- As a [‘data binder’](#binder) Erlsom can translate the XML document to an
  Erlang data structure that corresponds to an XML Schema. It has the
  advantage over the SAX parser that it validates the XML document, and
  that you know exactly what the layout of the output will be. This makes
  it easy to access the elements that you need in a very direct way. (Look
  [here](http://www.rpbourret.com/xml/XMLDataBinding.htm) for a general
  description of XML data binding.)

For all modes the following applies:
- If the document is too big to fit into memory, or if the document arrives
  in some kind of data stream, it can be passed to the parser in blocks of
  arbitrary size.

- The parser can work directly on binaries. There is no need to transform
  binaries to lists before passing the data to Erlsom. Using binaries as
  input has a positive effect on the memory usage and on the speed
  (provided that you are using Erlang 12B or later - if you are using an
  older Erlang version the speed will be better if you transform binaries
  to lists). The binaries can be latin-1, utf-8 or utf-16 encoded.

- The parser has an option to produce output in binary form (only the
  character data: names of elements and attributes are always strings).
  This may be convenient if you want to minimize the memory usage, and/or
  if you need the result in binary format for further processing. Note that it
  will slow down the parser slightly. If you select this option the encoding of
  the result will be utf-8 (irrespective of the encoding of the input document).

## <a name="example">Example XML document</a> ##
Unless otherwise indicated, the examples in the next sections will use the following, very simple XML document as input:

```xml
<foo attr="baz"><bar>x</bar><bar>y</bar></foo>
```

This document is stored in a file called "minimal.xml", and read into a
variable called Xml by the following commands in the shell:

```
1> {ok, Xml} = file:read_file("minimal.xml").
{ok,<<"<foo attr=\"baz\"><bar>x</bar><bar>y</bar></foo>\r\n">>}
```

The following, corresponding XSD ("minimal.xsd") is used in the first example for the data binder:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema">
    <xsd:element name="foo" type="foo_type"/>
    <xsd:complexType name="foo_type">
         <xsd:sequence>
             <xsd:element name="bar" type="xsd:string"
             maxOccurs="unbounded"/>
         </xsd:sequence>
         <xsd:attribute name="attr" type="xsd:string"/>
     </xsd:complexType>
</xsd:schema>
```

## <a name="sax">SAX Mode</a> ##
The example below shows how the [example XML](#example) can be processed
using the SAX parser:

```
2> erlsom:parse_sax(Xml, [], fun(Event, Acc) -> io:format("~p~n", [Event]), Acc end).
startDocument
{startElement,[],"foo",[],[{attribute,"attr",[],[],"baz"}]}
{startElement,[],"bar",[],[]}
{characters,"x"}
{endElement,[],"bar",[]}
{startElement,[],"bar",[],[]}
{characters,"y"}
{endElement,[],"bar",[]}
{endElement,[],"foo",[]}
endDocument
{ok,[],"\r\n"}
```

The function erlsom:parse_sax takes as its arguments: the XML document, an accumulator value and an ‘event processing function’. This function will process the parts of the XML documents that have been parsed. In this example, this function simply prints these events.

The next example does something slightly more meaningful: it counts the number of times the "bar" element occurs in the XML document. Ok, maybe not very useful, but at least this example will produce a result, not only side effects.

```
3> CountBar = fun(Event, Acc) -> case Event of {startElement, _, "bar", _, _} -> Acc + 1; _ -> Acc end end.
#Fun<erl_eval.12.113037538>

4> erlsom:parse_sax(Xml, 0, CountBar).
{ok,2,"\r\n"}
```

To describe it in a rather formal way: `parse_sax(Xml, Acc0, Fun)` calls Fun(Event, AccIn) on successive ‘XML events’ that result from parsing Xml, starting with AccIn == Acc0. Fun/2 must return a new accumulator which is passed to the next call. The function returns {ok, AccOut, Tail}, where AccOut is the final value of the accumulator and Tail the list of characters that follow after the last tag of the XML document. In this example AccOut == 2, since the tag occurs twice.
(Notice how similar this is to lists:foldl(Fun, Acc0, Sax\_events), assuming that Sax\_events is the list of Sax events - I more or less copied this description from the documentation of the lists module.)

It may still not be very clear to you how this SAX parser can be used to produce useful results. There are some additional examples in the examples directory of the Erlsom distribution. If you are still not convinced you can try to decipher the source code for the ‘data binder’ mode (erlsom_parse.erl) - this was also built on top of the SAX parser.

### <a name="sax_events">SAX Events</a> ##

#### startDocument

#### endDocument
Will NOT be sent out in case of an error

#### {startPrefixMapping, Prefix, URI}
Begin the scope of a prefix - URI namespace mapping
Will be sent immediately before the corresponding startElement event.

#### {endPrefixMapping, Prefix}
End the scope of a prefix - URI namespace mapping
Will be sent immediately before the corresponding endElement event.

#### {startElement, Uri, LocalName, Prefix, [Attributes]}
The beginning of an element.
There will be a corresponding endElement (even when the element is
empty).
All three name components will be provided.

[Attributes] is a list of attribute records, see sax.hrl.
Namespace attributes (xmlns:*) will not be reported.
There will be NO attribute values for defaulted attributes!

Providing 'Prefix' in stead of 'Qualified name' is probably not quite
in line with the SAX spec, but it appears to be more convenient.

#### {endElement, Uri, LocalName, Prefix}
The end of an element.

#### {characters, Characters}
Character data.
All character data will be in one chunk, except if there is a
CDATA section included inside a character section. In that case
there will be separate events for the characters before the CDATA, the
CDATA section and the characters following it (if any, of course).

#### {ignorableWhitespace, Characters}
If a character data section (as it would be reported by the 'characters'
event, see above) consists ONLY of whitespace, it will be
reported as ignorableWhitespace.

#### {processingInstruction, Target, Data}

#### {error, Description}

#### {internalError, Description}

## <a name="DOM">Simple DOM Mode</a> ##
This mode translates the XML document to a generic data structure. It doesn’t really follow the DOM standard, but in stead it provides a very simple format. In fact, it is very similar to format that is defined as the ‘simple-form’ in the Xmerl documentation.

An example will probably be sufficient to explain it:

```
erlsom:simple_form(Xml).
{ok,{"foo",
     [{"attr","baz"}],
     [{"bar",[],["x"]},{"bar",[],["y"]}]},
    "\r\n"}
```

Result = {ok, Element, Tail}, where Element = {Tag, Attributes, Content}, Tag is a string (there is an option that allows you to format Tag differently, see the reference section below), Attributes = [{AttributeName, Value}], and Content is a list of Elements and/or strings.

## <a name="binder">Data Binder Mode</a> ##
In this mode, Erlsom parses XML documents that are associated with an XSD (or Schema). It checks whether the XML document conforms to the Schema, and it translates the document to an Erlang structure that is based on the types defined in the Schema. This section tries to explain the relation between the Schema and the Erlang data structure that is produced by Erlsom.

First a quick example using the same XML that was used for the other modes. Before we can parse the document we need to ‘compile’ the XML Schema (similar to how you might compile a regular expression).

```
10> {ok, Model} = erlsom:compile_xsd_file("minimal.xsd").
{ok,{model,[{typ…
```

Now you can use this compiled model:

```
11> {ok, Result, _} = erlsom:scan(Xml, Model).
{ok,{foo_type,[],"baz",["x","y"]},"\r\n"}
```

Assuming that you have defined a suitable record #foo\_type{} (erlsom:write\_xsd\_hrl\_file() can do it for you), you can use in your program (won’t work in the shell):

```
BarValues = Result#foo_type.bar,
AttrValue = Result#foo_type.attr,
```

Nice and compact, as you see, but it may need more explanation. I will use a more complex example from the XML Schema  Primer ([XML Schema Part 0: Primer Second Edition](http://www.w3.org/TR/2004/REC-xmlschema-0-20041028/)).

```xml
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema">

  <xsd:annotation>
    <xsd:documentation xml:lang="en">
     Purchase order schema for Example.com.
     Copyright 2000 Example.com. All rights reserved.
    </xsd:documentation>
  </xsd:annotation>

  <xsd:element name="purchaseOrder" type="PurchaseOrderType"/>

  <xsd:element name="comment" type="xsd:string"/>

  <xsd:complexType name="PurchaseOrderType">
    <xsd:sequence>
      <xsd:element name="shipTo" type="USAddress"/>
      <xsd:element name="billTo" type="USAddress"/>
      <xsd:element ref="comment" minOccurs="0"/>
      <xsd:element name="items"  type="Items"/>
    </xsd:sequence>
    <xsd:attribute name="orderDate" type="xsd:date"/>
  </xsd:complexType>

  <xsd:complexType name="USAddress">
    <xsd:sequence>
      <xsd:element name="name"   type="xsd:string"/>
      <xsd:element name="street" type="xsd:string"/>
      <xsd:element name="city"   type="xsd:string"/>
      <xsd:element name="state"  type="xsd:string"/>
      <xsd:element name="zip"    type="xsd:decimal"/>
    </xsd:sequence>
    <xsd:attribute name="country" type="xsd:NMTOKEN"
                   fixed="US"/>
  </xsd:complexType>

  <xsd:complexType name="Items">
    <xsd:sequence>
      <xsd:element name="item" minOccurs="0" maxOccurs="unbounded">
        <xsd:complexType>
          <xsd:sequence>
            <xsd:element name="productName" type="xsd:string"/>
            <xsd:element name="quantity">
              <xsd:simpleType>
                <xsd:restriction base="xsd:positiveInteger">
                  <xsd:maxExclusive value="100"/>
                </xsd:restriction>
              </xsd:simpleType>
            </xsd:element>
            <xsd:element name="USPrice"  type="xsd:decimal"/>
            <xsd:element ref="comment"   minOccurs="0"/>
            <xsd:element name="shipDate" type="xsd:date" minOccurs="0"/>
          </xsd:sequence>
          <xsd:attribute name="partNum" type="SKU" use="required"/>
        </xsd:complexType>
      </xsd:element>
    </xsd:sequence>
  </xsd:complexType>

  <!-- Stock Keeping Unit, a code for identifying products -->
  <xsd:simpleType name="SKU">
    <xsd:restriction base="xsd:string">
      <xsd:pattern value="\d{3}-[A-Z]{2}"/>
    </xsd:restriction>
  </xsd:simpleType>

</xsd:schema>
```

*example 1: po.xsd*

This XSD can be processed by Erlsom: the compiler accepts it, and the parser can parse instances (XML documents) that conform to this schema.

Like the Primer, I will use po.xml as an example XML document.

```xml
<?xml version="1.0"?>
<purchaseOrder orderDate="1999-10-20">
   <shipTo country="US">
      <name>Alice Smith</name>
      <street>123 Maple Street</street>
      <city>Mill Valley</city>
      <state>CA</state>
      <zip>90952</zip>
   </shipTo>
   <billTo country="US">
      <name>Robert Smith</name>
      <street>8 Oak Avenue</street>
      <city>Old Town</city>
      <state>PA</state>
      <zip>95819</zip>
   </billTo>
   <comment>Hurry, my lawn is going wild<!/comment>
   <items>
      <item partNum="872-AA">
         <productName>Lawnmower</productName>
         <quantity>1</quantity>
         <USPrice>148.95</USPrice>
         <comment>Confirm this is electric</comment>
      </item>
      <item partNum="926-AA">
         <productName>Baby Monitor</productName>
         <quantity>1</quantity>
         <USPrice>39.98</USPrice>
         <shipDate>1999-05-21</shipDate>
      </item>
   </items>
</purchaseOrder>
```

*example 2: po.xml*

Translating po.xml using erlsom:scan/2 will result in:

```erlang
{'PurchaseOrderType',[],
                     "1999-10-20",
                     {'USAddress',[],
                                  "US",
                                  "Alice Smith",
                                  "123 Maple Street",
                                  "Mill Valley",
                                  "CA",
                                  "90952"},
                     {'USAddress',[],
                                  "US",
                                  "Robert Smith",
                                  "8 Oak Avenue",
                                  "Old Town",
                                  "PA",
                                  "95819"},
                     "Hurry, my lawn is going wild!",
                     {'Items',[],
                              [{'Items/item',
                                     [],
                                     "872-AA",
                                     "Lawnmower",
                                     "1",
                                     "148.95",
                                     "Confirm this is electric",
                                     undefined},
                               {'Items/item',
                                     [],
                                     "926-AA",
                                     "Baby Monitor",
                                     "1",
                                     "39.98",
                                     undefined,
                                     "1999-05-21"}]}}
```

*example 3: output for po.xml*

The output can be interpreted as a structure built from Erlang records. The
definition of these records can either be generated by
erlsom:write_xsd_hrl_file/3, or you can define them yourself (or a combination:
you can run write_xsd_hrl_file and change a few fieldnames and add some
defaults). An extract of the .hrl file generated by write_xsd_hrl_file/3:

```erlang
-record('USAddress', {anyAttribs :: anyAttribs(),
        country :: string() | undefined,
        name :: string(),
        street :: string(),
        city :: string(),
        state :: string(),
        zip :: string()}).

-type 'USAddress'() :: #'USAddress'{}.


-record('PurchaseOrderType', {anyAttribs :: anyAttribs(),
        orderDate :: string() | undefined,
        shipTo :: 'USAddress'(),
        billTo :: 'USAddress'(),
        comment :: string() | undefined,
        items :: 'Items'()}).

-type 'PurchaseOrderType'() :: #'PurchaseOrderType'{}.
```

*example 4: record definitions for po.xsd as generated by write_xsd_hrl_file/3*

As can be seen from the example:

-  attributes are included in the records as the first elements (country,
   partNum)
-  elements that are optional (minOccurs="0") for which no value is provided
   get the value undefined (comment, shipDate).
-  elements that can occur more than once (maxOccurs > 0 or unbounded) are
   translated to a list (listOfItem).
-  every record has ‘anyAttribs’ as its first element. If the Schema allows
   ‘anyAttributes’, and if these are present in the XML document, then the
   values will be found here (as a list of attribute-value pairs). Note
   that this can be avoided by passing the option {include_any_attribs,
   false} to erlsom:compile_xsd_file: in that case the ‘anyAttribs’ element
   will not be there.

It should be noted that there is quite a bit of information in po.xsd that is not used by erlsom:

-  Only in a limited number of situations does erlsom do type checking and
   translation: only if an element is defined as integer, int, boolean or QName
   without any further restrictions or extensions. The ‘quantity’ element doesn’t
   meet these conditions, since (a) it is a positiveInteger, and (b) it is
   restricted. A value for the quantity element of Ten or -1 would not result in
   an error or warning, and the string value is not translated to an Erlang
   integer. This also applies for the user defined simpleTypes, like SKU in the
   example.

   Note that this  behaviour can be modified by passing the option `{strict,
   true}` to erlsom:compile_xsd_file: in that case more types will be
   checked and mapped to Erlang types, for example `float` and `long`. This
   also applies to `positiveInteger`, but since the limitation that this
   only works for types without restrictions or extensions still holds, it would not
   make any difference for the value of ‘quantity’.

It should be noted that there is quite a bit of information in po.xsd that is not used by erlsom:

-  Only in a limited number of situations does erlsom do type checking and
   translation: only if an element is defined as integer, int, boolean or QName
   without any further restrictions or extensions. The ‘quantity’ element doesn’t
   meet these conditions, since (a) it is a positiveInteger, and (b) it is
   restricted. A value for the quantity element of Ten or -1 would not result in
   an error or warning, and the string value is not translated to an Erlang
   integer. This also applies for the user defined simpleTypes, like SKU in the
   example.
-  The fixed attribute is ignored. If there would have been another value than
   US in po.xml, this would have been accepted without warning or error.
-  The annotation is ignored (obviously).

In example 5 a number of additional features is illustrated:

-  elements that belong to a namespace are prefixed in the result. The prefix
   is determined by a parameter of the function that compiles the XSD.
-  anonymous types (in the example: spouse) get a name that include the ‘path’,
   in order to avoid name conflicts.
-  types (‘records’) are created for choices - the type indicates which
   alternative was selected (the record b:personType-hobby shows that "Mowing the lawn" is a hobby, not a profession).

```xml
<?xml version="1.0"?>
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            targetNamespace="http://www.example.org"
            xmlns="http://www.example.org"
            elementFormDefault="qualified">
  <xsd:element name="person" type="personType"/>
  <xsd:complexType name="personType">
     <xsd:sequence>
       <!-- an element with an attribute -->
       <xsd:element name = "id">
         <xsd:complexType>
           <xsd:simpleContent>
             <xsd:extension base = "xsd:string">
               <xsd:attribute name = "type" type = "xsd:string"/>
               <!-- allow other attributes-->
               <xsd:anyAttribute namespace="##other"/>
             </xsd:extension>
           </xsd:simpleContent>
         </xsd:complexType>
       </xsd:element>

       <!-- choice -->
       <xsd:choice>
         <xsd:element name="profession" type="xsd:string"/>
         <xsd:element name="hobby" type="xsd:string"/>
       </xsd:choice>

       <!-- group -->
       <xsd:group  ref="name"/>

       <!-- local type -->
       <xsd:element name="spouse" xsi:nillable="true">
         <xsd:complexType>
           <xsd:sequence>
             <xsd:element  name="name" type="xsd:string"/>
             <xsd:element  name="age" type="xsd:string"/>
           </xsd:sequence>
         </xsd:complexType>
       </xsd:element>

    </xsd:sequence>
  </xsd:complexType>

  <xsd:group name="name">
    <xsd:sequence>
      <!-- nillable element -->
      <xsd:element name="firstName"
                   type="xsd:string" nillable="true"/>
      <xsd:element name="lastName" type="xsd:string"/>
    </xsd:sequence>
  </xsd:group>

</xsd:schema>
```

*example 5: misc.xsd: namespace, choice, group*

```xml
<?xml version="1.0"?>
<person xmlns="http://www.example.org">
  <id type="passport">123</id>
  <hobby>mowing the lawn</hobby>
  <firstName>Jan</firstName>
  <lastName>Pietersen</lastName>
  <spouse>
      <name>Jet Pietersen</name>
      <age>33</age>
  </spouse>
</person>
```

*example 6: misc.xml*

The XSD can be compiled by the command

```
> {ok, Model} = erlsom:compile_xsd_file("misc.xsd",
                                        [{prefix, "b"}]).
```

After that the XML can be parsed using the command

```
> {ok, Out, Rest} = erlsom:scan_file("misc_example.xml", Model).
```

Out is the output shown below, and Rest is a string of the characters that may follow after the end tag of the XML.

```erlang
{'b:personType',[],
                {'b:personType/id',[], "passport","123"},
                {'b:personType-hobby',[], "mowing the lawn"},
                {'b:name',[], "Jan","Pietersen"},
                {'b:personType/spouse',[], "Jet Pietersen","33"}}
```

*example 7: output for misc.xml*

To show some additional features, have a look at the result of parsing misc_2.xml.

```xml
<?xml version="1.0"?>
<person xmlns="http://www.example.org"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:passport="some_uri">
  <id type="passport" passport:issued="2011">123</id>
  <hobby>mowing the lawn</hobby>
  <firstName xsi:nil="true"/>
  <lastName>Pietersen</lastName>
  <spouse xsi:nil="true"></spouse>
</person>
```

*example 8: misc_2.xml*

```erlang
{'b:personType',
   [],
   {'b:personType/id',
     [{{"issued","some_uri"},"2011"}],"passport","123"},
      {'b:personType-hobby',[],"mowing the lawn"},
      {'b:name',[],nil,"Pietersen"},
      {nil,{'b:personType/spouse',
            [{{"nil","http://www.w3.org/2001/XMLSchema-instance"},
              "true"}],
            undefined,
            undefined}}}
```

*example 9: output for misc_2.xml*

It shows how "nil" values are represented - note that there are 2 different cases: for a simple type the value `nil` is used, but for a complex type `{nil, Record}` is used, where Record is a record of the same type that would be used if a non-nil value would have been provided. The reason for this slightly awkward pattern is that a nil-value may still have attributes.

Example 9 also shows what happens to "any-attributes" ("issued", in this case): these are put into the first field of the record.

*A note about schema inclusion*

Be careful when working with complex schemas since Erlsom does not handle duplicate inclusions. You may end up with duplicate types!

However, duplicate inclusions can be resolved, e.g. by either editing the schemas or by detecting duplicate includes in a custom `include_fun` (which should then return an empty schema).

## <a name="installation">Installation</a>
The easiest way to install Erlsom is probably to use rebar.

Klacke (Claes Wickstrom) has provided a makefile. This should enable Unix users to install Erlsom easily.

Anyway,  even for Windows users and without rebar, installing erlsom should be straightforward. One way to do it is described below.

- Put all the files into the directory `ROOT/lib/erlsom-1.2.1/src`, where ROOT
  is the directory that contains Erlang (C:\Program Files\erl5.6.1 on my Windows system).
- Start the Erlang shell
- Change the working directory to `ROOT/lib/erlsom-1.2.1/src`:

```
1> cd('../lib/erlsom-1.2.1/src').
C:/Program Files/erl5.6.1/lib/erlsom-1.2.1/src
ok
```

- Compile the source files:

```
2> c("erlsom"),
c("erlsom_parse"),
c("erlsom_lib"),
c("erlsom_compile"),
c("erlsom_write"),
c("erlsom_parseXsd"),
c("erlsom_sax"),
c("erlsom_pass2"),
c("erlsom_writeHrl"),
c("erlsom_add"),
c("erlsom_ucs"),
c("erlsom_sax_utf8"),
c("erlsom_sax_latin1"),
c("erlsom_sax_latin9"),
c("erlsom_sax_utf16be"),
c("erlsom_sax_utf16le"),
c("erlsom_sax_list"),
c("erlsom_sax_lib"),
c("erlsom_simple_form").
```

- Move the .beam files to `ROOT/lib/erlsom-1.2.1/ebin`.

- Alternatively you can use emake for the last 2 steps:

```
2> make:all([{outdir, "../ebin"}]).
```


## <a name="examples">Examples</a> ##
The distribution includes 7 examples:

-  erlsom_example: this shows the use of the basic functions to compile an XSD,
   to parse an XML document and to write an XML document.

   To run the example from the Erlang shell: cd to the directory that contains the
   code (something like `cd('lib/erlsom-1.2.1/examples/erlsom_example').`),
   compile (`c("erlsom_example").`) and run (`erlsom_example:run().`).

- erlsom\_sax\_example: this shows the features of the SAX parser.

- example1: this example has 2 purposes:
   - It shows how easy Erlsom makes it for you to use an XML configuration
     file. The configuration file describes a set of 10 test cases, which are
     run by this example. The configuration file is described by "example1.xsd".
     Compiling this XSD and then parsing the configuration file ("example1.xml")
     gives you access to an Erlang structure of records that corresponds with the
     XML schema.

   - It shows how 11 different schemas (names "abb1.xsd" through "abb11.xsd")
     can describe the same XML document (named "abb.xml"), and it shows the output
     that results from running Erlsom on this file using these schema’s.
     To run the example for XSD abb1.xsd, use the command `example1:test_erlsom("abb1").`

- soap\_example: this shows how to use the `erlsom:add_xsd_file()` function,
  and it gives an example how you might parse and generate SOAP messages.

- continuation: this shows how to use the sax parser with a
  ‘continuation-function’. This can be used for parsing of very big files or
  streams. The continuation function should return a block of data; this will be
  parsed (calling the sax callback function when appropriate) and after that the
  function is called again to get the next block of data. The example shows how a
  file of arbitrary size can be parsed. The comments in the code should help you
  to understand and use this function.

- complex\_form: shows how you could create a back-end to the sax parser that
  produces the same output as Xmerl, and how you could then use the Xpath
  functions that Xmerl provides.

- book\_store; actually three examples, demonstrating the three modes that
  erlsom supports. The third example shows how you might combine different modes within
  a function that scans a file.

## <a name="encoding">Character encoding</a>
The sax parser accepts binaries as input. It will recognize UTF-8 and UTF-16 encoding by looking at the byte order mark and the first character of the document. Additionally ISO-8859-1 and ISO-8859-15 encoding is recognized if this is indicated by the XML declaration. If the XML declaration specifies another character set, an error will be thrown. It should not be very difficult to add support for other character sets, however.

As specified by the XML standard, the default encoding is UTF-8. If the first byte of the document is a ‘&lt;’ ASCII character and if the XML declaration does not specify anything else, it will be assumed that the encoding is UTF-8.

The result of erlsom:write is a list of Unicode code points. Normally this will have to be encoded before it can be used. The function erlsom\_ucs:to\_utf8/1 can be used to do this.

## <a name="atoms">Creation of atoms</a>
Especially in the context of internet applications, it may be a problem if new atoms are created as a result of communication based on XML (SOAP, XML-RPC, AJAX). The number of atoms that can be created within the Erlang runtime environment is limited, and uncontrolled creation of atoms may cause the system to crash.

Erlsom:scan/2 does not create new atoms. It uses string\_to\_existing\_atom to create the atoms that are used in the records.

Erlsom:compile\_xsd does create atoms. However, usually this function won’t be called with arbitrary end user input as its argument, so normally this should not be a problem.

## <a name="limitations">Limitations</a>
Some checks/validity constraints are accepted in the XSD, but not enforced during parsing:

- all simple types are interpreted as string. This applies to the built in
  types (float, positiveInteger, gYear etc), and also to types that are
  restricted (using 'facets') or extended (for example 'union' types). The only
  exceptions are Integer, Boolean and QName, these are translated.

  If the option `{strict, true}` is used when compiling the XSD, a number
  of additional types will be translated (and checked): Float, Double and
  all types that are derived from the Integer type, such as
  positiveInteger, nonNegativeInteger, Long, unsignedLong etc.

- Key, Unique etc. are not supported - if these elements occur in the XSD, they
  are simply ignored.

The SAX parser has the following limitations:

- It doesn’t support external entities.
- It doesn’t do any validation: if the XML includes a DTD, this is simply
  ignored.

The data binder has the following additional limitation:

- Names of elements and attributes cannot contain characters outside the Erlang
  character set (because they are translated to atoms).

### <a name="elements">XML Schema elements</a>
<table width="100%" border="1" cellspacing="0" cellpadding="2">
<tr><td valign="top">All</td><td>Supported. The parser puts the elements into the resulting record in a fixed place (independent of the order in which they are received).</td></tr>
<tr><td valign="top">Annotation</td><td>Ignored (anything enclosed in <documentation></documentation> is ignored).</td></tr>
<tr><td valign="top">Any</td><td>Supported. However, only elements that are included in the model will show up in the result. Elements are part of the model if they are included in the XSD that was compiled, or if they have been added using erlsom:add_file().</td></tr>
<tr><td valign="top">anyAttribute</td><td>Supported</td></tr>
<tr><td valign="top">Appinfo</td><td>Ignored (anything enclosed in <documentation></documentation> is ignored. </td></tr>
<tr><td valign="top">Attribute</td><td>Supported</td></tr>
<tr><td valign="top">attributeGroup</td><td>Supported</td></tr>
<tr><td valign="top">Choice</td><td>Supported</td></tr>
<tr><td valign="top">complexContent</td><td>Supported</td></tr>
<tr><td valign="top">complexType</td><td>Supported</td></tr>
<tr><td valign="top">Documentation</td><td>Accepted, but ignored. Anything enclosed in <code><documentation></documentation></code> is ignored (as long as it is valid XML).</td></tr>
<tr><td valign="top">Element</td><td>Supported</td></tr>
<tr><td valign="top">Enumeration</td><td>Ignored (all restrictions on simple types are ignored - those types are treated as ‘string’)</td></tr>
<tr><td valign="top">Extension</td><td>Supported</td></tr>
<tr><td valign="top">Field</td><td>Ignored (anything enclosed in <code><unique></unique></code> is ignored).</td></tr>
<tr><td valign="top">Group</td><td>Supported.</td></tr>
<tr><td valign="top">Import</td><td>Supported. However, the support for finding the imported files is limited. See (and modify, if necessary...) the function findFile in erlsom_lib.erl. </td></tr>
<tr><td valign="top">Include</td><td>Supported. However, the support for finding the included files is limited. See (and modify, if necessary...) the function findFile in erlsom_lib.erl.</td></tr>
<tr><td valign="top">Key</td><td>Ignored.</td></tr>
<tr><td valign="top">Keyref</td><td>Ignored</td></tr>
<tr><td valign="top">Length</td><td>Ignored (all restrictions on simple types are ignored - those types are treated as ‘string’)</td></tr>
<tr><td valign="top">List</td><td>Ignored (all restrictions on simple types are ignored - those types are treated as ‘string’)</td></tr>
<tr><td valign="top">maxInclusive</td><td>(all restrictions on simple types are ignored - those types are treated as ‘string’)</td></tr>
<tr><td valign="top">maxLength</td><td>(see maxInclusive) </td></tr>
<tr><td valign="top">minInclusive</td><td>(see maxInclusive) </td></tr>
<tr><td valign="top">minLength</td><td>(see maxInclusive) </td></tr>
<tr><td valign="top">Pattern</td><td>(see maxInclusive) </td></tr>
<tr><td valign="top">Redefine</td><td>Supported. However, the support for finding the imported files is limited. See (and modify, if necessary...) the function findFile in erlsom_lib.erl.</td></tr>
<tr><td valign="top">Restriction</td><td>Supported as a way to create a derived complex type (but it is not checked whether this is really a restriction of the base type). Ignored on simpleTypes (all restrictions on simple types are ignored - those types are treated as ‘string’)</td></tr>
<tr><td valign="top">Schema</td><td>Supported</td></tr>
<tr><td valign="top">Selector</td><td>Ignored (anything enclosed in <code><unique></unique></code> is ignored).</td></tr>
<tr><td valign="top">Sequence</td><td>Supported</td></tr>
<tr><td valign="top">simpleContent</td><td>Supported</td></tr>
<tr><td valign="top">simpleType</td><td>Supported</td></tr>
<tr><td valign="top">Union</td><td>Ignored (all restrictions on simple types are ignored - those types are treated as ‘string’)</td></tr>
<tr><td valign="top">Unique</td><td>Ignored</td></tr>
</table>

### <a name="attributes">XML Schema Attributes</a>

<table width="100%" border="1" cellspacing="0" cellpadding="2">
<tr><td valign="top">Abstract</td><td>ignored. As a consequence, the parser may accept documents that contain instances of abstract types.</td></tr>
<tr><td valign="top">attributeFormDefault</td><td>Supported.</td></tr>
<tr><td valign="top">Block</td><td>not supported</td></tr>
<tr><td valign="top">blockDefault</td><td>ignored </td></tr>
<tr><td valign="top">Default</td><td>ignored (note that this not just a check that is not performed: the default value will not be provided)</td></tr>
<tr><td valign="top">Final</td><td>Ignored</td></tr>
<tr><td valign="top">finalDefault</td><td>Ignored</td></tr>
<tr><td valign="top">Fixed</td><td>Ignored</td></tr>
<tr><td valign="top">Form</td><td>not supported</td></tr>
<tr><td valign="top">Mixed</td><td>Supported (text values are inserted into a list of values) </td></tr>
<tr><td valign="top">minOccurs, maxOccurs</td><td>supported, except on group definitions</td></tr>
<tr><td valign="top">namespace (for 'any')</td><td>supported, but lists of values are not supported (##any, ##local and ##other are supported). A list of values is treated as '##any'.</td></tr>
<tr><td valign="top">schemaLocation</td><td>supported in a limited way, see 'import'.</td></tr>
<tr><td valign="top">xsi:schemaLocation</td><td>Ignored</td></tr>
<tr><td valign="top">substitutionGroup</td><td>Supported</td></tr>
<tr><td valign="top">Type</td><td>supported, but there is no check on the built-in types, except for integer, int, boolean and QName.</td></tr>
<tr><td valign="top">Use</td><td>supported, but 'prohibited' is ignored (treated as 'optional').</td></tr>
</table>

## Reference ##
See [Reference](doc/reference.md).

