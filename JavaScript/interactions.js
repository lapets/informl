var I = imparse;
var grammar =
  I.Grammar([
    I.Production('Formula', [
      I.Choices([
        I.Choice('And', I.AssocNone(), [
          I.Nonterminal('Formula'), I.Terminal('and'), I.Nonterminal('Formula')
        ]),
        I.Choice('Or', I.AssocNone(), [
          I.Nonterminal('Formula'), I.Terminal('or'), I.Nonterminal('Formula')
        ]),
        I.Choice('Equal', I.AssocNone(), [
          I.Nonterminal('Number'), I.Terminal('=='), I.Nonterminal('Number')
        ]),
        I.Choice('Not', I.AssocNone(), [
          I.Terminal('not'), I.Nonterminal('Formula')
        ]),
        I.Choice('True', I.AssocNone(), [
          I.Terminal('true')
        ]),
        I.Choice('False', I.AssocNone(), [
          I.Terminal('false')
        ]),
      ]),
    ]),
    I.Production('Number', [
      I.Choices([
        I.Choice('Plus', I.AssocNone(), [
          I.Nonterminal('Number'), I.Terminal('+'), I.Nonterminal('Number')
        ]),
        I.Choice('Minus', I.AssocNone(), [
          I.Nonterminal('Number'), I.Terminal('-'), I.Nonterminal('Number')
        ]),
        I.Choice('Number', I.AssocNone(), [
          I.RegExpr('(0|[1-9][0-9]*)')
        ]),
      ]),
    ]),
  ]);

var uxadtStr = {"tree":null};

var procBNF = function () {
  var str = document.getElementById('bnf_input').value;
  var ux = I.bnfTouxadt_old(str);
  grammar = ux;
  uxadtStr["tree"] = I.printuxadt_oldGrammar(ux,2,"_Imparse");
  document.getElementById('bnf_output1').innerHTML = uxadtStr["tree"];
};

var procParse = function () {
  var str = document.getElementById('parser_input').value;
  var tree = I.parser(grammar, str);
  if (typeof tree == "string") {
  document.getElementById('parser_output').innerHTML = tree;
  } else {
  document.getElementById('parser_output').innerHTML = JSON.stringify(tree, null, 2);
  }
};

var procIntp = function() {
  if (uxadtStr["tree"] == null || typeof uxadtStr["tree"] != 'string') return;
  var newstr = uxadtStr["tree"].replace(/_Imparse./g, "Type.");
  eval(("var ux=".concat(newstr).concat(";")));
  var pr = ImparseToInforml.evalGrammar(ux);
  document.getElementById('interpret_out').value = pr;
}

function sendToServer() {
  var posting = $.post("http:\\\\localhost:8888", function(){alert("hit")});
  var result = posting.done(function( data ) {
    var content = $( data ).find( "#content" );
    $( "#result" ).empty().append( content );
  });
}