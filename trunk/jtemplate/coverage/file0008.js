var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 38</i>';
states['fold000001'] = false;
texts['fold000040'] = '<a href="javascript:fold(\'fold000040\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 40 to line 57</i>';
states['fold000040'] = false;
texts['fold000059'] = '<a href="javascript:fold(\'fold000059\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 59 to line 70</i>';
states['fold000059'] = false;
texts['fold000072'] = '<a href="javascript:fold(\'fold000072\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 72 to line 78</i>';
states['fold000072'] = false;
texts['fold000080'] = '<a href="javascript:fold(\'fold000080\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 80 to line 90</i>';
states['fold000080'] = false;
texts['fold000092'] = '<a href="javascript:fold(\'fold000092\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 92 to line 95</i>';
states['fold000092'] = false;
texts['fold000098'] = '<a href="javascript:fold(\'fold000098\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 98 to line 108</i>';
states['fold000098'] = false;
texts['fold000110'] = '<a href="javascript:fold(\'fold000110\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 110 to line 113</i>';
states['fold000110'] = false;
texts['fold000116'] = '<a href="javascript:fold(\'fold000116\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 116 to line 130</i>';
states['fold000116'] = false;
texts['fold000133'] = '<a href="javascript:fold(\'fold000133\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 133 to line 147</i>';
states['fold000133'] = false;
texts['fold000151'] = '<a href="javascript:fold(\'fold000151\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 151 to line 166</i>';
states['fold000151'] = false;
texts['fold000170'] = '<a href="javascript:fold(\'fold000170\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 170 to line 179</i>';
states['fold000170'] = false;
texts['fold000182'] = '<a href="javascript:fold(\'fold000182\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 182 to line 185</i>';
states['fold000182'] = false;
texts['fold000188'] = '<a href="javascript:fold(\'fold000188\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 188 to line 198</i>';
states['fold000188'] = false;
texts['fold000200'] = '<a href="javascript:fold(\'fold000200\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 200 to line 250</i>';
states['fold000200'] = false;
texts['fold000252'] = '<a href="javascript:fold(\'fold000252\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 252 to line 264</i>';
states['fold000252'] = false;
texts['fold000266'] = '<a href="javascript:fold(\'fold000266\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 266 to line 282</i>';
states['fold000266'] = false;
texts['fold000284'] = '<a href="javascript:fold(\'fold000284\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 284 to line 286</i>';
states['fold000284'] = false;
texts['fold000288'] = '<a href="javascript:fold(\'fold000288\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 288 to line 302</i>';
states['fold000288'] = false;

function fold(id) {
  tmp = document.all[id].innerHTML;
  document.all[id].innerHTML = texts[id];
  texts[id] = tmp;
  states[id] = !(states[id]);
}

function unfoldAll() {
  for (key in states) {
    if (states[key]) {
      fold(key);
    }
  }
}

function foldAll() {
  for (key in states) {
    if (!(states[key])) {
      fold(key);
    }
  }
}
