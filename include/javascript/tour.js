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
 */SUGAR.tour=function(){var tourModal;return{init:function(params){var modals=params.modals;tourModal=$('<div id="'+params.id+'" class="modal"></div>').modal({backdrop:false}).draggable({handle:".modal-header"});var tourIdSel="#"+params.id;$.ajax({url:params.modalUrl,success:function(data){$(tourIdSel).html(data);$(tourIdSel+'Start a.btn.btn-primary').on("click",function(e){$(tourIdSel+'Start').css("display","none");$(tourIdSel+'End').css("display","block");tourModal.modal("hide");modalArray[0].modal('show');$(modals[0].target).popoverext('show');});$(tourIdSel+'Start a.btn').not('.btn-primary').on("click",function(e){$(tourIdSel+'Start').css("display","none");$(tourIdSel+'End').css("display","block");centerModal();});$(tourIdSel+' a.close').on("click",function(e){tourModal.modal("hide");SUGAR.tour.saveUserPref(params.prefUrl);params.onTourFinish();});$(tourIdSel+'End a.btn.btn-primary').on("click",function(e){tourModal.modal("hide");SUGAR.tour.saveUserPref(params.prefUrl);params.onTourFinish();});centerModal();$('<div style="position: absolute;" id="tourArrow">arrow</div>');var modalArray=new Array();for(var i=0;i<modals.length;i++){var modalId=modals[i].target.replace("#","")+"_modal";modalArray[i]=$('<div id="'+modalId+'" class="modal '+params.className+'"></div>').modal({backdrop:false}).draggable({handle:".modal-header"});var modalContent="<div class=\"modal-header\"><a class=\"close\" data-dismiss=\"modal\">×</a><h3>"+modals[i].title+"</h3></div>";modalContent+="<div class=\"modal-body\">"+modals[i].content+"</div>";modalContent+=footerTemplate(i,modals);$('#'+modalId).html(modalContent);modalArray[i].modal("hide");$(modals[i].target).ready(function(){var direction,bounce;if(modals[i].placement=="top right"){bounce="up right";direction="down right"}else if(modals[i].placement=="top left"){bounce="up left";direction="down left"}else if(modals[i].placement=="top"){bounce="up";direction="down"}else if(modals[i].placement=="bottom right"){bounce="down right";direction="up right"}else if(modals[i].placement=="bottom left"){bounce="down left";direction="up left"}else{bounce="down";direction="right"}
$(modals[i].target).popoverext({title:"",content:"arrow",footer:"",placement:modals[i].placement,id:true,fixed:true,trigger:'manual',template:'<div class="popover arrow"><div class="pointer '+direction+'"></div></div>',onShow:function(){$('.pointer').css('top','0px');$(".popover .pointer").effect("custombounce",{times:1000,direction:bounce,distance:50,gravity:false},2000,function(){});},leftOffset:modals[i].leftOffset?modals[i].leftOffset:0,topOffset:modals[i].topOffset?modals[i].topOffset:0,hideOnBlur:true});});$(modals[i].target+"Popover").empty().html("arrow");}
$(window).resize(function(){centerModal();});function centerModal(){$(tourIdSel).css("left",$(window).width()/2-$(tourIdSel).width()/2);$(tourIdSel).css("margin-top",-$(tourIdSel).height()/2);}
function nextModal(i){if(modals.length-1!=i){$(modals[i].target).popoverext('hide');$(modals[i+1].target).popoverext('show');modalArray[i].modal('hide');modalArray[i+1].modal('show');}else{$(modals[i].target).popoverext('hide');modalArray[i].modal('hide');tourModal.modal("show");centerModal();}}
function prevModal(i){$(modals[i].target).popoverext('hide');$(modals[i-1].target).popoverext('show');modalArray[i].modal('hide');modalArray[i-1].modal('show');}
function skipTour(i){$(modals[i].target).popoverext('hide');modalArray[i].modal('hide');tourModal.modal("show");centerModal();}
function footerTemplate(i,modals){var content=$('<div></div>')
var footer=$("<div class=\"modal-footer\"></div>");var skip=$("<a href=\"#\" class=\"btn btn-invisible\" id=\"skipTour\">"+SUGAR.language.get('app_strings','LBL_TOUR_SKIP')+"</a>");var next=$('<a class="btn btn-primary" id="nextModal'+i+'" href="#">'+SUGAR.language.get('app_strings','LBL_TOUR_NEXT')+' <i class="icon-play icon-xsm"></i></a>');content.append(footer);footer.append(skip).append(next);var back=$('<a class="btn" href="#" id="prevModal'+i+'">'+SUGAR.language.get('app_strings','LBL_TOUR_BACK')+'</a>');$('#nextModal'+i).live("click",function(){nextModal(i);});$('#prevModal'+i).live("click",function(){prevModal(i);});$('#skipTour').live("click",function(){skipTour(i);});if(i!=0){footer.append(back);}
return content.html();}}});},saveUserPref:function(url){$.ajax({type:"GET",url:url});}};}();