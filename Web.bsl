me Web

use Core.Happstack as Core.Happstack

def (o) f@ g@ x@ = f (g x)

type Attribute Attr [@, @]

def attribute attr@ val@ =
  Attr [attr, val]

def class     attribute "class"
def href      attribute "href"
def httpEquiv attribute "httpEquiv"
def name      attribute "name"
def rel       attribute "rel"
def src       attribute "src"
def style     attribute "style"
def type#     attribute "type#"
def width     attribute "width"

type Html Tag @

def tag
  fn@ val@isString = Core.Happstack.applyTag fn (Core.Happstack.string val)
| fn@ (Attribute [attr@, val@]) =
    (x@ = Core.Happstack.applyAttribute (tag fn x) attr val)
| fn@ val@ = Core.Happstack.applyTag fn val

def a           tag Core.Happstack.a
def abbr        tag Core.Happstack.abbr
def address     tag Core.Happstack.address
def area        tag Core.Happstack.area
def article     tag Core.Happstack.article
def aside       tag Core.Happstack.aside
def audio       tag Core.Happstack.audio
def b           tag Core.Happstack.b
def base        tag Core.Happstack.base
def bdo         tag Core.Happstack.bdo
def blockquote  tag Core.Happstack.blockquote
def body        tag Core.Happstack.body
def br          tag Core.Happstack.br
def button      tag Core.Happstack.button
def canvas      tag Core.Happstack.canvas
def caption     tag Core.Happstack.caption
def cite        tag Core.Happstack.cite
def code        tag Core.Happstack.code
def col         tag Core.Happstack.col
def colgroup    tag Core.Happstack.colgroup
def command     tag Core.Happstack.command
def datalist    tag Core.Happstack.datalist
def dd          tag Core.Happstack.dd
def del         tag Core.Happstack.del
def details     tag Core.Happstack.details
def dfn         tag Core.Happstack.dfn
def dl          tag Core.Happstack.dl
def docType     tag Core.Happstack.docType
def docTypeHtml tag Core.Happstack.docTypeHtml
def dt          tag Core.Happstack.dt
def em          tag Core.Happstack.em
def embed       tag Core.Happstack.embed
def fieldset    tag Core.Happstack.fieldset
def figcaption  tag Core.Happstack.figcaption
def figure      tag Core.Happstack.figure
def footer      tag Core.Happstack.footer
def form        tag Core.Happstack.form
def h1          tag Core.Happstack.h1
def h2          tag Core.Happstack.h2
def h3          tag Core.Happstack.h3
def h4          tag Core.Happstack.h4
def h5          tag Core.Happstack.h5
def h6          tag Core.Happstack.h6
def head        tag Core.Happstack.head
def header      tag Core.Happstack.header
def hgroup      tag Core.Happstack.hgroup
def hr          tag Core.Happstack.hr
def html        tag Core.Happstack.html
def i           tag Core.Happstack.i
def iframe      tag Core.Happstack.iframe
def img         tag Core.Happstack.img
def input       tag Core.Happstack.input
def ins         tag Core.Happstack.ins
def kbd         tag Core.Happstack.kbd
def keygen      tag Core.Happstack.keygen
def label       tag Core.Happstack.label
def legend      tag Core.Happstack.legend
def li          tag Core.Happstack.li
def link        tag Core.Happstack.link
def map         tag Core.Happstack.map
def mark        tag Core.Happstack.mark
def menu        tag Core.Happstack.menu
def meta        tag Core.Happstack.meta
def meter       tag Core.Happstack.meter
def nav         tag Core.Happstack.nav
def noscript    tag Core.Happstack.noscript
def object      tag Core.Happstack.object
def ol          tag Core.Happstack.ol
def optgroup    tag Core.Happstack.optgroup
def option      tag Core.Happstack.option
def output      tag Core.Happstack.output
def p           tag Core.Happstack.p
def param       tag Core.Happstack.param
def pre         tag Core.Happstack.pre
def progress    tag Core.Happstack.progress
def q           tag Core.Happstack.q
def rp          tag Core.Happstack.rp
def rt          tag Core.Happstack.rt
def ruby        tag Core.Happstack.ruby
def samp        tag Core.Happstack.samp
def script      tag Core.Happstack.script
def section     tag Core.Happstack.section
def select      tag Core.Happstack.select
def small       tag Core.Happstack.small
def source      tag Core.Happstack.source
def span        tag Core.Happstack.span
def strong      tag Core.Happstack.strong
def style       tag Core.Happstack.style
-- def sub         tag Core.Happstack.sub
def summary     tag Core.Happstack.summary
def sup         tag Core.Happstack.sup
def table       tag Core.Happstack.table
def tbody       tag Core.Happstack.tbody
def td          tag Core.Happstack.td
def textarea    tag Core.Happstack.textarea
def tfoot       tag Core.Happstack.tfoot
def th          tag Core.Happstack.th
def thead       tag Core.Happstack.thead
def time        tag Core.Happstack.time
def title       tag Core.Happstack.title
def tr          tag Core.Happstack.tr
def ul          tag Core.Happstack.ul
def var         tag Core.Happstack.var
def video       tag Core.Happstack.video

def page
  html (body (p "ola tudo bem"))

def main
  Core.Happstack.serve page
