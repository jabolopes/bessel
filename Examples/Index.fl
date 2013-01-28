me Examples.Index

use List

def info :=
  div:<id:"info",class:"floatright">:
    <h1:<>:"Jose A Lopes",
     addr,
     contact>
  where {
    def addr := intercalate:<br,street>
      where {
        def street := <"MPI-SWS","Campus E1 4","D-66123 Saarbrücken","Germany">
      }

    def contact :=
      p:<>:
        <em:<>:"Phone:",
         "+49 681 9303-8911",
         br,
         em:<>:"Email:",
         "jose 'at' mpi-sws 'dot' org">
  }

def infoDiv := div:<>:<info,picture>
  where { def picture := img:<width:"200px",src:"jose.jpg",class:"floatleft"> }

def p1 := p:<style:"padding-top: 20px">:<t1,link1,t2,link2,".">
  where {
    def t1 :=
      "I am a PhD student at Max Planck Institute for Software Systems ("

    def link1 :=
       a:<href:"http://www.mpi-sws.org/">:"MPI-SWS"

    def t2 :=
       ". I work in the Synthesis, Analysis and Automated Reasoning group. My advisor is "

    def link2 :=
      a:<href:"http://www.mpi-sws.org/~piskac">:"Ruzica Piskac"
  }

def p2 := p:<>:<intercalate:<" ",t>,link,".">
  where {
    def t :=
      <"I completed BSc in Software Engineering and Information",
       "Systems and MSc in Software Engineering and Multimedia, both",
       "degrees at">

    def link :=
      a:<href:"http://www.ist.utl.pt">:"Instituto Superior Técnico, Technical University of Lisbon"
  }

def p3 := p:<>:<t1,link,intercalate:<" ",t2>>
  where {
    def t1 :=
      <"I also worked in several research groups at ">

    def link :=
      a:<href:"http://www.inesc-id.pt">:"INESC-ID"

    def t2 :=
      <" during my BSc and MSc. My research interests are programming",
       "languages, type systems, and verification.">
  }

def p4 := p:<>:(intercalate:<br,<a1,a2,a3,a4>>)
  where {
    def a1 := a:<href:"cv.pdf">:"Curriculum Vitae"
    def a2 := a:<href:"http://www.linkedin.com/in/jabolopes">:"LinkedIn"
    def a3 := a:<href:"http://www.github.com/jabolopes/">:"GitHub"
    def a4 := a:<href:"http://fenix.ist.utl.pt/homepage/ist158612">:"MSc thesis work"
  }

def content :=  
  div:<id:"content",class:"nonfloating">:
    <p1,
     p2,
     p3,
     p4>

def footer :=
  div:<id:"footer">:
    <p:<>:"Updated: December 1, 2012",
     layout>
  where {
    def layout :=
      p:<>:<"Page layout by ",
            a:<href:"http://www.mpi-sws.org/~fniksic">:"Filip Niksic",
            ".">
  }

def body1 :=
  body:<>:<
    div:<id:"container">:
      <infoDiv,
       content,
       footer>
  >

def header :=
  head:<>:<
    meta:<httpEquiv:"content-type",content:"text/html; charset=UTF-8">,
    meta:<httpEquiv:"content-type",content:"text/html; charset=UTF-8">,
    meta:<name:"author",content:"Jose A. Lopes">,
    meta:<name:"description",content:"Jose A. Lopes">,
    title:<>:"Jose A. Lopes",
    link:<href:"style.css",rel:"stylesheet",type_:"text/css">:<>
  >

def html1 :=
  html:<>:<header,body1>