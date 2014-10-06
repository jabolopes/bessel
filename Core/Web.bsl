me Core.Web

use Core.Html

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
    mkTag (Html.applyAttribute tag attr val)
| tag@ val@isString =
    mkSingleTag (Html.applyTag tag (Html.string val))
| tag@ vals@((const true) +> (const true)) =
    mkSingleTag (Html.applyList tag vals)
| tag@ val@ =
    mkSingleTag (Html.applyTag tag val)

def (!)
  tag@ (Attribute [attr@, val@]) =
    mkSingleTag (Html.applyAttribute tag attr val)

def a           mkTag Html.a
def abbr        mkTag Html.abbr
def address     mkTag Html.address
def area        mkSingleTag Html.area
def article     mkTag Html.article
def aside       mkTag Html.aside
def audio       mkTag Html.audio
def b           mkTag Html.b
def base        mkSingleTag Html.base
def bdo         mkTag Html.bdo
def blockquote  mkTag Html.blockquote
def body        mkTag Html.body
def br          mkSingleTag Html.br
def button      mkTag Html.button
def canvas      mkTag Html.canvas
def caption     mkTag Html.caption
def cite        mkTag Html.cite
def code        mkTag Html.code
def col         mkSingleTag Html.col
def colgroup    mkTag Html.colgroup
def command     mkTag Html.command
def datalist    mkTag Html.datalist
def dd          mkTag Html.dd
def del         mkTag Html.del
def details     mkTag Html.details
def dfn         mkTag Html.dfn
def dl          mkTag Html.dl
def docType     mkSingleTag Html.docType
def docTypeHtml mkTag Html.docTypeHtml
def dt          mkTag Html.dt
def em          mkTag Html.em
def embed       mkSingleTag Html.embed
def fieldset    mkTag Html.fieldset
def figcaption  mkTag Html.figcaption
def figure      mkTag Html.figure
def footer      mkTag Html.footer
def form        mkTag Html.form
def h1          mkTag Html.h1
def h2          mkTag Html.h2
def h3          mkTag Html.h3
def h4          mkTag Html.h4
def h5          mkTag Html.h5
def h6          mkTag Html.h6
def head        mkTag Html.head
def header      mkTag Html.header
def hgroup      mkTag Html.hgroup
def hr          mkSingleTag Html.hr
def html        mkTag Html.html
def i           mkTag Html.i
def iframe      mkTag Html.iframe
def img         mkSingleTag Html.img
def input       mkSingleTag Html.input
def ins         mkTag Html.ins
def kbd         mkTag Html.kbd
def keygen      mkSingleTag Html.keygen
def label       mkTag Html.label
def legend      mkTag Html.legend
def li          mkTag Html.li
def link        mkSingleTag Html.link
def map         mkTag Html.map
def mark        mkTag Html.mark
def menu        mkTag Html.menu
def meta        mkSingleTag Html.meta
def meter       mkTag Html.meter
def nav         mkTag Html.nav
def noscript    mkTag Html.noscript
def object      mkTag Html.object
def ol          mkTag Html.ol
def optgroup    mkTag Html.optgroup
def option      mkTag Html.option
def output      mkTag Html.output
def p           mkTag Html.p
def param       mkSingleTag Html.param
def pre         mkTag Html.pre
def progress    mkTag Html.progress
def q           mkTag Html.q
def rp          mkTag Html.rp
def rt          mkTag Html.rt
def ruby        mkTag Html.ruby
def samp        mkTag Html.samp
def script      mkTag Html.script
def section     mkTag Html.section
def select      mkTag Html.select
def small       mkTag Html.small
def source      mkSingleTag Html.source
def span        mkTag Html.span
def strong      mkTag Html.strong
-- def style       tag Html.style
-- def sub         tag Html.sub
def summary     mkTag Html.summary
def sup         mkTag Html.sup
def table       mkTag Html.table
def tbody       mkTag Html.tbody
def td          mkTag Html.td
def textarea    mkTag Html.textarea
def tfoot       mkTag Html.tfoot
def th          mkTag Html.th
def thead       mkTag Html.thead
def time        mkTag Html.time
def title       mkTag Html.title
def tr          mkTag Html.tr
def ul          mkTag Html.ul
def var         mkTag Html.var
def video       mkTag Html.video

def renderHtml Html.renderHtml
