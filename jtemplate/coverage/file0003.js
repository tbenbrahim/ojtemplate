var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 44</i>';
states['fold000001'] = false;
texts['fold000046'] = '<a href="javascript:fold(\'fold000046\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 46 to line 59</i>';
states['fold000046'] = false;
texts['fold000061'] = '<a href="javascript:fold(\'fold000061\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 61 to line 75</i>';
states['fold000061'] = false;
texts['fold000077'] = '<a href="javascript:fold(\'fold000077\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 77 to line 93</i>';
states['fold000077'] = false;
texts['fold000096'] = '<a href="javascript:fold(\'fold000096\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 96 to line 105</i>';
states['fold000096'] = false;
texts['fold000107'] = '<a href="javascript:fold(\'fold000107\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 107 to line 124</i>';
states['fold000107'] = false;
texts['fold000126'] = '<a href="javascript:fold(\'fold000126\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 126 to line 135</i>';
states['fold000126'] = false;
texts['fold000137'] = '<a href="javascript:fold(\'fold000137\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 137 to line 148</i>';
states['fold000137'] = false;
texts['fold000150'] = '<a href="javascript:fold(\'fold000150\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 150 to line 154</i>';
states['fold000150'] = false;
texts['fold000156'] = '<a href="javascript:fold(\'fold000156\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 156 to line 161</i>';
states['fold000156'] = false;
texts['fold000163'] = '<a href="javascript:fold(\'fold000163\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 163 to line 168</i>';
states['fold000163'] = false;
texts['fold000170'] = '<a href="javascript:fold(\'fold000170\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 170 to line 170</i>';
states['fold000170'] = false;

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
