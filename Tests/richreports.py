#####################################################################
## 
## richreports.py
##
##   A library that supports the manual and automated assembly of
##   modules for building interactive HTML reports consisting of
##   abstract syntax trees as concrete syntax annotated with the
##   results of static analysis and abstract interpretation
##   algorithms.
##
##   Web:     richreports.org
##   Version: 0.0.3.0
##
##

import uxadt
_ = None

#####################################################################
## Rich report data structure definitions.
##

uxadt._({
  'HighlightUnbound': [],
  'HighlightUnreachable': [],
  'HighlightDuplicate': [],
  'HighlightError': [],
  'Highlight': ['$']
  })

uxadt._({
  'Lt': [],
  'Gt': [],
  'Space': [],
  'Ampersand': []
  })

uxadt._({
  'Entity': ['Entity'],
  'Text': ['$'],
  'Symbol': ['$'],
  'Punctuation': ['$'],
  'Keyword': ['$'],
  'Literal': ['$'],
  'Konstant': ['$'], # "Constant" is a reserved word in some cases.
  'Operator': ['$'],
  'Builtin': ['$'],
  'Library': ['$'],
  'Variable': ['$'],
  'Error': ['$'],

  'Atom': [['Highlight'], ['Report'], ['Report']],
  'Span': [['Highlight'], ['Report'], ['Report']],
  'Line': [['Report']],
  'Block': [['Highlight'], ['Report'], ['Report']],

  'Concat': [['Report']],
  'Intersperse': ['Report', ['Report']],
  'Field': [['Report']],
  'Row': [['Report']],
  'Table': [['Report']],

  'Page': ['Report']
  })

#####################################################################
## Interactive HTML report rendering functions.
##

def highlightsStr(hs): return ' '.join([' '.join(highlightStr(h)) for h in hs])
def highlightStr(h):
  return h\
    ._(HighlightUnbound(),     lambda: ['RichReports_Highlight_Unbound'])\
    ._(HighlightUnreachable(), lambda: ['RichReports_Highlight_Unreachable'])\
    ._(HighlightDuplicate(),   lambda: ['RichReports_Highlight_Duplicate'])\
    ._(HighlightError(),       lambda: ['RichReports_Highlight_Error'])\
    ._(Highlight(_),           lambda hs: hs)\
    .end
    
def entityStr(e):
  return e\
    ._(Lt(),        lambda: '&lt;')\
    ._(Gt(),        lambda: '&gt;')\
    ._(Space(),     lambda: '&nbsp;')\
    ._(Ampersand(), lambda: '&amp;')\
    .end

def messagesToAttr(ms):
  def conv(m):
    return '\''+html(m)\
      .replace('"', '&quot;')\
      .replace('\'', '\\\'')\
      .replace('\n', '')\
      .replace('\r', '')\
      + '\''
  return 'onclick="msg(this, ['+ ','.join([conv(m) for m in ms]) +']);"'

def html(r):
  def conc(rs): return ''.join([html(r) for r in rs])
  print(r)
  return r\
    ._(Entity(_), lambda e: '<span class="RichReports_Entity">' + entityStr(e) + '</span>')\
    ._(Text(_), lambda s: '<span class="RichReports_Text">' + s + '</span>')\
    ._(Symbol(_), lambda s: '<span class="RichReports_Symbol">' + s + '</span>')\
    ._(Punctuation(_), lambda s: '<span class="RichReports_Punctuation">' + s + '</span>')\
    ._(Keyword(_), lambda s: '<span class="RichReports_Keyword">' + s + '</span>')\
    ._(Literal(_), lambda s: '<span class="RichReports_Literal">' + s + '</span>')\
    ._(Konstant(_), lambda s: '<span class="RichReports_Konstant">' + s + '</span>')\
    ._(Operator(_), lambda s: '<span class="RichReports_Operator">' + s + '</span>')\
    ._(Builtin(_), lambda s: '<span class="RichReports_Builtin">' + s + '</span>')\
    ._(Library(_), lambda s: '<span class="RichReports_Library">' + s + '</span>')\
    ._(Variable(_), lambda s: '<span class="RichReports_Variable">' + s + '</span>')\
    ._(Error(_), lambda s: '<span class="RichReports_Error">' + s + '</span>')\
    ._(Atom(_,_,_), lambda hs,ms,rs:\
      '<span><span class="RichReports_Clickable" ' +\
      messagesToAttr(ms) +\
      '>'+\
      '<span class="' + ('RichReports_Highlight' if len(hs) > 0 or len(ms) > 0 else '') + ' ' + highlightsStr(hs) +'">' +\
      conc(rs) +\
      '</span></span></span>'\
      if ms\
      else '<span class="'+ highlightsStr(hs) +'">' + conc(rs) + '</span>'\
      )\
    ._(Span(_,_,_), lambda hs,ms,rs:\
      '<span><span class="RichReports_Clickable RichReports_Clickable_Exclamation" '+messagesToAttr(ms)+'>!</span><span class="'+ highlightsStr(hs) +'">' + conc(rs) + '</span></span>'\
      if ms\
      else '<span class="'+ highlightsStr(hs) +'">' + conc(rs) + '</span>'\
      )\
    ._(Line(_), lambda rs: '<div>' + conc(rs) + '</div>')\
    ._(Block(_,_,_), lambda hs,ms,rs: '<div class="RichReports_Block">' + conc(rs) + '</div>')\
    ._(Concat(_), conc)\
    ._(Intersperse(_,_), lambda r,rs: html(r).join([html(r0) for r0 in rs]))\
    ._(Field(_), lambda rs: '<td>'+ conc(rs) + '</td>')\
    ._(Row(_), lambda rs: '<tr>' + conc(rs) + '</tr>')\
    ._(Table(_), lambda rs: '<table>' + conc(rs) + '</table>')\
    ._(Page(_), lambda r: '''
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <style>
      body {
        font-family: "Courier", monospace;
        font-size: 12px;
      }
      table {
        font-family: "Courier", monospace;
        font-size: 12px;
      }
      #RichReports_Message {
        background-color: yellow;
        padding: 3px;
        border: 1px solid black;
        font-family: "Courier", monospace;
        font-size: 12px;
        cursor: pointer;
      }
      .RichReports_Clickable {
        cursor: pointer;
      }
      .RichReports_Clickable_Exclamation {
        background-color: yellow;
        border: 1px solid black;
        margin: 0px 5px 1px 5px;
        padding: 0px 2px 0px 2px;
        font-size: 9px;
      }
      .RichReports_Clickable:hover {
        background-color: yellow;
      }

      .RichReports_Entity      { }
      .RichReports_Text        { }
      .RichReports_Symbol      { font-weight: bold;  color: black; }
      .RichReports_Punctuation { font-weight: bold;  color: black; }
      .RichReports_Keyword     { font-weight: bold;  color: blue; }
      .RichReports_Literal     { font-weight: bold;  color: firebrick; }
      .RichReports_Konstant    { font-weight: bold;  color: blue; }
      .RichReports_Operator    { font-weight: bold;  color: blue; }
      .RichReports_Builtin     { font-weight: bold;  color: purple; }
      .RichReports_Library     { font-weight: bold;  color: purple; }
      .RichReports_Variable    { font-style: italic; color: green; }
      .RichReports_Error {
        font-weight: bold;
        color: red;
        text-decoration: underline;
      }
      .RichReports_Highlight             { margin:           2px;       }
      .RichReports_Highlight_Unbound     { background-color: orange;    }
      .RichReports_Highlight_Unreachable { background-color: orange;    }
      .RichReports_Highlight_Duplicate   { background-color: yellow;    }
      .RichReports_Highlight_Error       { background-color: lightpink; }
      .RichReports_Block                 { margin-left:      10px;      }
    </style>
    <script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.3/jquery.min.js"></script>
    <script type="text/javascript">
      function msg (obj, msgs) {
        var html = '';
        for (var i = 0; i < msgs.length; i++)
          html += '<div class="RichReports_MessagePortion">' + msgs[i] + '</div>';
        document.getElementById('RichReports_Message').innerHTML = html;
        document.getElementById('RichReports_Message').style.display = 'inline-block';
        var top = $(obj).offset().top;
        var left = $(obj).offset().left;
        $('#RichReports_Message').offset({top:top + 15, left:left + 15});
      }
    </script>
  </head>
  <body>
          ''' + html(r) + '''
    <div id="RichReports_Message" style="display:none;" onclick="this.style.display='none';"></div>
  </body>
</html>
      ''')\
    .end

##eof