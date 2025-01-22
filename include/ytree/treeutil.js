/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */function treeinit(tree,treedata,treediv,params){tree=new YAHOO.widget.TreeView(treediv);YAHOO.namespace(treediv).param=params;var root=tree.getRoot();var tmpNode;var data=treedata;for(nodedata in data){for(node in data[nodedata]){addNode(root,data[nodedata][node]);}}
tree.subscribe("clickEvent",function(o){set_selected_node(this.id,null,o.node);});tree.draw();}
function set_selected_node(treeid,nodeid,node){if(typeof(node)=='undefined')
node=YAHOO.widget.TreeView.getNode(treeid,nodeid);if(typeof(node)!='undefined'){YAHOO.namespace(treeid).selectednode=node;}else{YAHOO.namespace(treeid).selectednode=null;}}
function addNode(parentnode,nodedef){var dynamicload=false;var dynamicloadfunction;var childnodes;var expanded=false;if(nodedef['data']!='undefined'){if(typeof(nodedef['custom'])!='undefined'){dynamicload=nodedef['custom']['dynamicload'];dynamicloadfunction=nodedef['custom']['dynamicloadfunction'];expanded=nodedef['custom']['expanded'];}
var tmpNode=new YAHOO.widget.TextNode(nodedef['data'],parentnode,expanded);if(dynamicload){SUGAR.util.globalEval('e=('+dynamicloadfunction+')');tmpNode.setDynamicLoad(e);}
if(typeof(nodedef['nodes'])!='undefined'){for(childnodes in nodedef['nodes']){addNode(tmpNode,nodedef['nodes'][childnodes]);}}}}
function node_click(treeid,target,targettype,functioname){node=YAHOO.namespace(treeid).selectednode;var url=site_url.site_url+"/index.php?entryPoint=TreeData&function="+functioname+construct_url_from_tree_param(node);var callback={success:function(o){var targetdiv=document.getElementById(o.argument[0]);targetdiv.innerHTML=o.responseText;SUGAR.util.evalScript(o.responseText);},failure:function(o){},argument:[target,targettype]}
var trobj=YAHOO.util.Connect.asyncRequest('GET',url,callback,null);}
function construct_url_from_tree_param(node){var treespace=YAHOO.namespace(node.tree.id);url="&PARAMT_depth="+node.depth;if(treespace!='undefined'){for(tparam in treespace.param){url=url+"&PARAMT_"+tparam+'='+treespace.param[tparam];}}
for(i=node.depth;i>=0;i--){var currentnode;if(i==node.depth){currentnode=node;}else{currentnode=node.getAncestor(i);}
url=url+"&PARAMN_id"+'_'+currentnode.depth+'='+currentnode.data.id;if(currentnode.data.param!='undefined'){for(nparam in currentnode.data.param){url=url+"&PARAMN_"+nparam+'_'+currentnode.depth+'='+currentnode.data.param[nparam];}}}
return url;}
function loadDataForNode(node,onCompleteCallback){var id=node.data.id;var params="entryPoint=TreeData&function=get_node_data"+construct_url_from_tree_param(node);var callback={success:function(o){node=o.argument[0];var nodes=YAHOO.lang.JSON.parse(o.responseText);var tmpNode;for(nodedata in nodes){for(node1 in nodes[nodedata]){addNode(node,nodes[nodedata][node1]);}}
o.argument[1]();},failure:function(o){},argument:[node,onCompleteCallback]}
var trobj=YAHOO.util.Connect.asyncRequest('POST','index.php',callback,params);}