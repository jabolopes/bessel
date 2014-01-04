me Web

use Core.Happstack as Core.Happstack

def (o) f@ g@ x@ = f (g x)

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
    mkTag (Core.Happstack.applyAttribute tag attr val)
| tag@ val@isString =
    mkSingleTag (Core.Happstack.applyTag tag (Core.Happstack.string val))
| tag@ vals@((const true) +> (const true)) =
    mkSingleTag (Core.Happstack.applyList tag vals)
| tag@ val@ =
    mkSingleTag (Core.Happstack.applyTag tag val)

def (!)
  tag@ (Attribute [attr@, val@]) =
    mkSingleTag (Core.Happstack.applyAttribute tag attr val)

def a           mkTag Core.Happstack.a
def abbr        mkTag Core.Happstack.abbr
def address     mkTag Core.Happstack.address
def area        mkSingleTag Core.Happstack.area
def article     mkTag Core.Happstack.article
def aside       mkTag Core.Happstack.aside
def audio       mkTag Core.Happstack.audio
def b           mkTag Core.Happstack.b
def base        mkSingleTag Core.Happstack.base
def bdo         mkTag Core.Happstack.bdo
def blockquote  mkTag Core.Happstack.blockquote
def body        mkTag Core.Happstack.body
def br          mkSingleTag Core.Happstack.br
def button      mkTag Core.Happstack.button
def canvas      mkTag Core.Happstack.canvas
def caption     mkTag Core.Happstack.caption
def cite        mkTag Core.Happstack.cite
def code        mkTag Core.Happstack.code
def col         mkSingleTag Core.Happstack.col
def colgroup    mkTag Core.Happstack.colgroup
def command     mkTag Core.Happstack.command
def datalist    mkTag Core.Happstack.datalist
def dd          mkTag Core.Happstack.dd
def del         mkTag Core.Happstack.del
def details     mkTag Core.Happstack.details
def dfn         mkTag Core.Happstack.dfn
def dl          mkTag Core.Happstack.dl
def docType     mkSingleTag Core.Happstack.docType
def docTypeHtml mkTag Core.Happstack.docTypeHtml
def dt          mkTag Core.Happstack.dt
def em          mkTag Core.Happstack.em
def embed       mkSingleTag Core.Happstack.embed
def fieldset    mkTag Core.Happstack.fieldset
def figcaption  mkTag Core.Happstack.figcaption
def figure      mkTag Core.Happstack.figure
def footer      mkTag Core.Happstack.footer
def form        mkTag Core.Happstack.form
def h1          mkTag Core.Happstack.h1
def h2          mkTag Core.Happstack.h2
def h3          mkTag Core.Happstack.h3
def h4          mkTag Core.Happstack.h4
def h5          mkTag Core.Happstack.h5
def h6          mkTag Core.Happstack.h6
def head        mkTag Core.Happstack.head
def header      mkTag Core.Happstack.header
def hgroup      mkTag Core.Happstack.hgroup
def hr          mkSingleTag Core.Happstack.hr
def html        mkTag Core.Happstack.html
def i           mkTag Core.Happstack.i
def iframe      mkTag Core.Happstack.iframe
def img         mkSingleTag Core.Happstack.img
def input       mkSingleTag Core.Happstack.input
def ins         mkTag Core.Happstack.ins
def kbd         mkTag Core.Happstack.kbd
def keygen      mkSingleTag Core.Happstack.keygen
def label       mkTag Core.Happstack.label
def legend      mkTag Core.Happstack.legend
def li          mkTag Core.Happstack.li
def link        mkSingleTag Core.Happstack.link
def map         mkTag Core.Happstack.map
def mark        mkTag Core.Happstack.mark
def menu        mkTag Core.Happstack.menu
def meta        mkSingleTag Core.Happstack.meta
def meter       mkTag Core.Happstack.meter
def nav         mkTag Core.Happstack.nav
def noscript    mkTag Core.Happstack.noscript
def object      mkTag Core.Happstack.object
def ol          mkTag Core.Happstack.ol
def optgroup    mkTag Core.Happstack.optgroup
def option      mkTag Core.Happstack.option
def output      mkTag Core.Happstack.output
def p           mkTag Core.Happstack.p
def param       mkSingleTag Core.Happstack.param
def pre         mkTag Core.Happstack.pre
def progress    mkTag Core.Happstack.progress
def q           mkTag Core.Happstack.q
def rp          mkTag Core.Happstack.rp
def rt          mkTag Core.Happstack.rt
def ruby        mkTag Core.Happstack.ruby
def samp        mkTag Core.Happstack.samp
def script      mkTag Core.Happstack.script
def section     mkTag Core.Happstack.section
def select      mkTag Core.Happstack.select
def small       mkTag Core.Happstack.small
def source      mkSingleTag Core.Happstack.source
def span        mkTag Core.Happstack.span
def strong      mkTag Core.Happstack.strong
-- def style       tag Core.Happstack.style
-- def sub         tag Core.Happstack.sub
def summary     mkTag Core.Happstack.summary
def sup         mkTag Core.Happstack.sup
def table       mkTag Core.Happstack.table
def tbody       mkTag Core.Happstack.tbody
def td          mkTag Core.Happstack.td
def textarea    mkTag Core.Happstack.textarea
def tfoot       mkTag Core.Happstack.tfoot
def th          mkTag Core.Happstack.th
def thead       mkTag Core.Happstack.thead
def time        mkTag Core.Happstack.time
def title       mkTag Core.Happstack.title
def tr          mkTag Core.Happstack.tr
def ul          mkTag Core.Happstack.ul
def var         mkTag Core.Happstack.var
def video       mkTag Core.Happstack.video

-- def main
--   Core.Happstack.serveDir "/home/jose/Projects/jabolopes.github.com/"

def app x@ =
  Core.Happstack.serveDirectory "/home/jose/Projects/jabolopes.github.com"

def main
  Core.Happstack.serveApp app
