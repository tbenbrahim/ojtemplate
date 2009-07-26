var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 17</i>';
states['fold000001'] = false;
texts['fold000019'] = '<a href="javascript:fold(\'fold000019\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 19 to line 19</i>';
states['fold000019'] = false;
texts['fold000021'] = '<a href="javascript:fold(\'fold000021\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 21 to line 29</i>';
states['fold000021'] = false;
texts['fold000032'] = '<a href="javascript:fold(\'fold000032\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 32 to line 36</i>';
states['fold000032'] = false;
texts['fold000038'] = '<a href="javascript:fold(\'fold000038\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 38 to line 43</i>';
states['fold000038'] = false;
texts['fold000046'] = '<a href="javascript:fold(\'fold000046\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 46 to line 69</i>';
states['fold000046'] = false;
texts['fold000071'] = '<a href="javascript:fold(\'fold000071\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 71 to line 72</i>';
states['fold000071'] = false;
texts['fold000075'] = '<a href="javascript:fold(\'fold000075\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 75 to line 75</i>';
states['fold000075'] = false;
texts['fold000080'] = '<a href="javascript:fold(\'fold000080\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 80 to line 86</i>';
states['fold000080'] = false;
texts['fold000088'] = '<a href="javascript:fold(\'fold000088\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 88 to line 88</i>';
states['fold000088'] = false;
texts['fold000090'] = '<a href="javascript:fold(\'fold000090\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 90 to line 95</i>';
states['fold000090'] = false;
texts['fold000097'] = '<a href="javascript:fold(\'fold000097\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 97 to line 143</i>';
states['fold000097'] = false;
texts['fold000145'] = '<a href="javascript:fold(\'fold000145\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 145 to line 156</i>';
states['fold000145'] = false;
texts['fold000158'] = '<a href="javascript:fold(\'fold000158\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 158 to line 160</i>';
states['fold000158'] = false;
texts['fold000164'] = '<a href="javascript:fold(\'fold000164\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 164 to line 175</i>';
states['fold000164'] = false;
texts['fold000179'] = '<a href="javascript:fold(\'fold000179\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 179 to line 183</i>';
states['fold000179'] = false;
texts['fold000185'] = '<a href="javascript:fold(\'fold000185\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 185 to line 198</i>';
states['fold000185'] = false;
texts['fold000200'] = '<a href="javascript:fold(\'fold000200\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 200 to line 203</i>';
states['fold000200'] = false;
texts['fold000205'] = '<a href="javascript:fold(\'fold000205\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 205 to line 205</i>';
states['fold000205'] = false;
texts['fold000214'] = '<a href="javascript:fold(\'fold000214\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 214 to line 218</i>';
states['fold000214'] = false;
texts['fold000222'] = '<a href="javascript:fold(\'fold000222\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 222 to line 353</i>';
states['fold000222'] = false;
texts['fold000355'] = '<a href="javascript:fold(\'fold000355\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 355 to line 386</i>';
states['fold000355'] = false;
texts['fold000388'] = '<a href="javascript:fold(\'fold000388\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 388 to line 404</i>';
states['fold000388'] = false;

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
