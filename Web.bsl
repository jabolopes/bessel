me Web

use Core.Html as Core.Html

type Attribute Attr [@, @]

def attribute attr@ val@ =
  Attr [attr, val]

def class      attribute "class"
def content    attribute "content"
def href       attribute "href"
def http-equiv attribute "httpEquiv"
def name       attribute "name"
def rel        attribute "rel"
def src        attribute "src"
def style      attribute "style"
def type#      attribute "type#"
def width      attribute "width"

def mkSingleTag id

def mkTag
  tag@ (Attribute [attr@, val@]) =
    mkTag (Core.Html.applyAttribute tag attr val)
| tag@ val@isString =
    mkSingleTag (Core.Html.applyTag tag (Core.Html.string val))
| tag@ vals@((const true) +> (const true)) =
    mkSingleTag (Core.Html.applyList tag vals)
| tag@ val@ =
    mkSingleTag (Core.Html.applyTag tag val)

def (!)
  tag@ (Attribute [attr@, val@]) =
    mkSingleTag (Core.Html.applyAttribute tag attr val)

def a           mkTag Core.Html.a
def abbr        mkTag Core.Html.abbr
def address     mkTag Core.Html.address
def area        mkSingleTag Core.Html.area
def article     mkTag Core.Html.article
def aside       mkTag Core.Html.aside
def audio       mkTag Core.Html.audio
def b           mkTag Core.Html.b
def base        mkSingleTag Core.Html.base
def bdo         mkTag Core.Html.bdo
def blockquote  mkTag Core.Html.blockquote
def body        mkTag Core.Html.body
def br          mkSingleTag Core.Html.br
def button      mkTag Core.Html.button
def canvas      mkTag Core.Html.canvas
def caption     mkTag Core.Html.caption
def cite        mkTag Core.Html.cite
def code        mkTag Core.Html.code
def col         mkSingleTag Core.Html.col
def colgroup    mkTag Core.Html.colgroup
def command     mkTag Core.Html.command
def datalist    mkTag Core.Html.datalist
def dd          mkTag Core.Html.dd
def del         mkTag Core.Html.del
def details     mkTag Core.Html.details
def dfn         mkTag Core.Html.dfn
def dl          mkTag Core.Html.dl
def docType     mkSingleTag Core.Html.docType
def docTypeHtml mkTag Core.Html.docTypeHtml
def dt          mkTag Core.Html.dt
def em          mkTag Core.Html.em
def embed       mkSingleTag Core.Html.embed
def fieldset    mkTag Core.Html.fieldset
def figcaption  mkTag Core.Html.figcaption
def figure      mkTag Core.Html.figure
def footer      mkTag Core.Html.footer
def form        mkTag Core.Html.form
def h1          mkTag Core.Html.h1
def h2          mkTag Core.Html.h2
def h3          mkTag Core.Html.h3
def h4          mkTag Core.Html.h4
def h5          mkTag Core.Html.h5
def h6          mkTag Core.Html.h6
def head        mkTag Core.Html.head
def header      mkTag Core.Html.header
def hgroup      mkTag Core.Html.hgroup
def hr          mkSingleTag Core.Html.hr
def html        mkTag Core.Html.html
def i           mkTag Core.Html.i
def iframe      mkTag Core.Html.iframe
def img         mkSingleTag Core.Html.img
def input       mkSingleTag Core.Html.input
def ins         mkTag Core.Html.ins
def kbd         mkTag Core.Html.kbd
def keygen      mkSingleTag Core.Html.keygen
def label       mkTag Core.Html.label
def legend      mkTag Core.Html.legend
def li          mkTag Core.Html.li
def link        mkSingleTag Core.Html.link
def map         mkTag Core.Html.map
def mark        mkTag Core.Html.mark
def menu        mkTag Core.Html.menu
def meta        mkSingleTag Core.Html.meta
def meter       mkTag Core.Html.meter
def nav         mkTag Core.Html.nav
def noscript    mkTag Core.Html.noscript
def object      mkTag Core.Html.object
def ol          mkTag Core.Html.ol
def optgroup    mkTag Core.Html.optgroup
def option      mkTag Core.Html.option
def output      mkTag Core.Html.output
def p           mkTag Core.Html.p
def param       mkSingleTag Core.Html.param
def pre         mkTag Core.Html.pre
def progress    mkTag Core.Html.progress
def q           mkTag Core.Html.q
def rp          mkTag Core.Html.rp
def rt          mkTag Core.Html.rt
def ruby        mkTag Core.Html.ruby
def samp        mkTag Core.Html.samp
def script      mkTag Core.Html.script
def section     mkTag Core.Html.section
def select      mkTag Core.Html.select
def small       mkTag Core.Html.small
def source      mkSingleTag Core.Html.source
def span        mkTag Core.Html.span
def strong      mkTag Core.Html.strong
-- def style       tag Core.Html.style
-- def sub         tag Core.Html.sub
def summary     mkTag Core.Html.summary
def sup         mkTag Core.Html.sup
def table       mkTag Core.Html.table
def tbody       mkTag Core.Html.tbody
def td          mkTag Core.Html.td
def textarea    mkTag Core.Html.textarea
def tfoot       mkTag Core.Html.tfoot
def th          mkTag Core.Html.th
def thead       mkTag Core.Html.thead
def time        mkTag Core.Html.time
def title       mkTag Core.Html.title
def tr          mkTag Core.Html.tr
def ul          mkTag Core.Html.ul
def var         mkTag Core.Html.var
def video       mkTag Core.Html.video

def renderHtml Core.Html.renderHtml
