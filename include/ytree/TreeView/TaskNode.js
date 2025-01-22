/**
 * The check box marks a task complete.  It is a simulated form field
 * with three states ...
 * 0=unchecked, 1=some children checked, 2=all children checked
 * When a task is clicked, the state of the nodes and parent and children
 * are updated, and this behavior cascades.
 *
 * @extends YAHOO.widget.TextNode
 * @constructor
 * @param oData    {object}  A string or object containing the data that will
 *                           be used to render this node.
 * @param oParent  {Node}    This node's parent node
 * @param expanded {boolean} The initial expanded/collapsed state
 * @param checked  {boolean} The initial checked/unchecked state
 */YAHOO.widget.TaskNode=function(oData,oParent,expanded,checked){if(oData){this.init(oData,oParent,expanded);this.setUpLabel(oData);this.setUpCheck(checked);}};YAHOO.widget.TaskNode.prototype=new YAHOO.widget.TextNode();YAHOO.widget.TaskNode.prototype.checked=false;YAHOO.widget.TaskNode.prototype.checkState=0;YAHOO.widget.TaskNode.prototype.cascadeUp=false;YAHOO.widget.TaskNode.prototype.taskNodeParentChange=function(){if(this.tree&&!this.tree.hasEvent("checkClick")){this.tree.createEvent("checkClick",this.tree);}};YAHOO.widget.TaskNode.prototype.setUpCheck=function(checked){if(checked&&checked===true){this.check();}
this.taskNodeParentChange();this.subscribe("parentChange",this.taskNodeParentChange);};YAHOO.widget.TaskNode.prototype.getCheckElId=function(){return"ygtvcheck"+this.index;};YAHOO.widget.TaskNode.prototype.getCheckEl=function(){return document.getElementById(this.getCheckElId());};YAHOO.widget.TaskNode.prototype.getCheckStyle=function(){return"ygtvcheck"+this.checkState;};YAHOO.widget.TaskNode.prototype.getCheckLink=function(){return"YAHOO.widget.TreeView.getNode(\'"+this.tree.id+"\',"+
this.index+").checkClick()";};YAHOO.widget.TaskNode.prototype.checkClick=function(){if(this.checkState===0){this.check();}else{this.uncheck();}
this.onCheckClick();this.tree.fireEvent("checkClick",this);};YAHOO.widget.TaskNode.prototype.onCheckClick=function(){};YAHOO.widget.TaskNode.prototype.updateParent=function(){var p=this.parent;if(!p||!p.updateParent){return;}
var somethingChecked=false;var somethingNotChecked=false;for(var i=0;i<p.children.length;++i){if(p.children[i].checked){somethingChecked=true;if(p.children[i].checkState==1){somethingNotChecked=true;}}else{somethingNotChecked=true;}}
if(somethingChecked){p.setCheckState((somethingNotChecked)?1:2);}else{p.setCheckState(0);}
p.updateCheckHtml();p.updateParent();};YAHOO.widget.TaskNode.prototype.updateCheckHtml=function(){if(this.parent&&this.parent.childrenRendered){this.getCheckEl().className=this.getCheckStyle();}};YAHOO.widget.TaskNode.prototype.setCheckState=function(state){this.checkState=state;this.checked=(state>0);};YAHOO.widget.TaskNode.prototype.check=function(){this.setCheckState(2);for(var i=0;i<this.children.length;++i){this.children[i].check();}
this.updateCheckHtml();if(this.cascadeUp)
this.updateParent();};YAHOO.widget.TaskNode.prototype.uncheck=function(){this.setCheckState(0);for(var i=0;i<this.children.length;++i){this.children[i].uncheck();}
this.updateCheckHtml();this.updateParent();};YAHOO.widget.TaskNode.prototype.getNodeHtml=function(){var sb=new Array();sb[sb.length]='<table border="0" cellpadding="0" cellspacing="0">';sb[sb.length]='<tr>';for(var i=0;i<this.depth;++i){sb[sb.length]='<td class="'+this.getDepthStyle(i)+'">&#160;</td>';}
sb[sb.length]='<td';sb[sb.length]=' id="'+this.getToggleElId()+'"';sb[sb.length]=' class="'+this.getStyle()+'"';if(this.hasChildren(true)){sb[sb.length]=' onmouseover="this.className=';sb[sb.length]='YAHOO.widget.TreeView.getNode(\'';sb[sb.length]=this.tree.id+'\','+this.index+').getHoverStyle()"';sb[sb.length]=' onmouseout="this.className=';sb[sb.length]='YAHOO.widget.TreeView.getNode(\'';sb[sb.length]=this.tree.id+'\','+this.index+').getStyle()"';}
sb[sb.length]=' onclick="javascript:'+this.getToggleLink()+'">&#160;';sb[sb.length]='</td>';sb[sb.length]='<td';sb[sb.length]=' id="'+this.getCheckElId()+'"';sb[sb.length]=' class="'+this.getCheckStyle()+'"';sb[sb.length]=' onclick="javascript:'+this.getCheckLink()+'">';sb[sb.length]='&#160;</td>';sb[sb.length]='<td>';sb[sb.length]='<a';sb[sb.length]=' id="'+this.labelElId+'"';sb[sb.length]=' class="'+this.labelStyle+'"';sb[sb.length]=' href="'+this.href+'"';sb[sb.length]=' target="'+this.target+'"';if(this.hasChildren(true)){sb[sb.length]=' onmouseover="document.getElementById(\'';sb[sb.length]=this.getToggleElId()+'\').className=';sb[sb.length]='YAHOO.widget.TreeView.getNode(\'';sb[sb.length]=this.tree.id+'\','+this.index+').getHoverStyle()"';sb[sb.length]=' onmouseout="document.getElementById(\'';sb[sb.length]=this.getToggleElId()+'\').className=';sb[sb.length]='YAHOO.widget.TreeView.getNode(\'';sb[sb.length]=this.tree.id+'\','+this.index+').getStyle()"';}
sb[sb.length]=' >';sb[sb.length]=this.label;sb[sb.length]='</a>';sb[sb.length]='</td>';sb[sb.length]='</tr>';sb[sb.length]='</table>';return sb.join("");};YAHOO.widget.TaskNode.prototype.toString=function(){return"TaskNode ("+this.index+") "+this.label;};