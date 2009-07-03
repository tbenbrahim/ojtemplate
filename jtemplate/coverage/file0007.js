var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 32</i>';
states['fold000001'] = false;
texts['fold000035'] = '<a href="javascript:fold(\'fold000035\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 35 to line 40</i>';
states['fold000035'] = false;
texts['fold000043'] = '<a href="javascript:fold(\'fold000043\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 43 to line 50</i>';
states['fold000043'] = false;
texts['fold000053'] = '<a href="javascript:fold(\'fold000053\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 53 to line 74</i>';
states['fold000053'] = false;
texts['fold000076'] = '<a href="javascript:fold(\'fold000076\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 76 to line 77</i>';
states['fold000076'] = false;
texts['fold000081'] = '<a href="javascript:fold(\'fold000081\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 81 to line 82</i>';
states['fold000081'] = false;
texts['fold000085'] = '<a href="javascript:fold(\'fold000085\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 85 to line 86</i>';
states['fold000085'] = false;
texts['fold000088'] = '<a href="javascript:fold(\'fold000088\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 88 to line 99</i>';
states['fold000088'] = false;
texts['fold000101'] = '<a href="javascript:fold(\'fold000101\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 101 to line 105</i>';
states['fold000101'] = false;
texts['fold000107'] = '<a href="javascript:fold(\'fold000107\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 107 to line 109</i>';
states['fold000107'] = false;
texts['fold000111'] = '<a href="javascript:fold(\'fold000111\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 111 to line 120</i>';
states['fold000111'] = false;
texts['fold000122'] = '<a href="javascript:fold(\'fold000122\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 122 to line 129</i>';
states['fold000122'] = false;
texts['fold000131'] = '<a href="javascript:fold(\'fold000131\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 131 to line 147</i>';
states['fold000131'] = false;
texts['fold000150'] = '<a href="javascript:fold(\'fold000150\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 150 to line 151</i>';
states['fold000150'] = false;
texts['fold000153'] = '<a href="javascript:fold(\'fold000153\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 153 to line 153</i>';
states['fold000153'] = false;
texts['fold000155'] = '<a href="javascript:fold(\'fold000155\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 155 to line 179</i>';
states['fold000155'] = false;
texts['fold000181'] = '<a href="javascript:fold(\'fold000181\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 181 to line 187</i>';
states['fold000181'] = false;
texts['fold000189'] = '<a href="javascript:fold(\'fold000189\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 189 to line 199</i>';
states['fold000189'] = false;
texts['fold000201'] = '<a href="javascript:fold(\'fold000201\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 201 to line 202</i>';
states['fold000201'] = false;
texts['fold000204'] = '<a href="javascript:fold(\'fold000204\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 204 to line 222</i>';
states['fold000204'] = false;

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