
/**
 * CustomCourseMenu Block Helper - Courses.js
 *
 * @package    block_custom_course_menu
 * @copyright  2015 onwards University of Portland (www.up.edu)
 * @license    http://www.gnu.org/copyleft/gpl.html GNU GPL v3 or later
 */
(function(factory){if(typeof define==="function"&&define.amd){require.config({waitSeconds:30});require(["jquery","jqueryui"],factory)}else{factory(jQuery)}}(function($){var interfaceUrl=$('#custom_course_menu_interface').attr('href');var $container=$('#custom_course_menu_dynamic');var saveSortsFor=function(type,$ul){var url=$('#custom_course_menu_sort').text();var regex=new RegExp(type+"id=([\\w-]{1,25})");var ids=[];var sortorder=[];var sessionid=M.cfg.sesskey;$ul.children().each(function(index,elem){var href=$(elem).find('.item_tool').attr('href');ids.push(regex.exec(href)[1]);sortorder.push(index)});type=type=='course'&&$ul.find('.favorite_courses').length?'favorite':type;var params={type:type,sesskey:sessionid,ids:ids,sortorder:sortorder.join(","),datetime:Date.now(),};return $.ajax({url:url,data:params,type:"POST",})};var sortableObject={cursor:"move",handle:".handle",axis:"y",placeholder:"placeholder",update:function(event,$ui){var $ul=$ui.item.parent();var type=$ui.item.attr('class').match(/category/)?'category':'course';saveSortsFor(type,$ul)}};var categorySwitcher=function(){var $this=$(this);var $list=$this.siblings('.custom_course_menu_list');var collapsed=$this.hasClass('minus');var toAdd="";var toRemove="";var listAdd="";var listRemove="";if(collapsed){toAdd='plus';toRemove='minus';listAdd='collapsed';listRemove='not_collapsed'}else{toAdd='minus';toRemove='plus';listAdd='not_collapsed';listRemove='collapsed'}
$this.removeClass(toRemove).addClass(toAdd);$list.removeClass(listRemove).addClass(listAdd);$this.html($('#custom_course_menu_'+toAdd).html());var sessionid=M.cfg.sesskey;var params={sesskey:sessionid,};$.ajax({url:$this.attr('href'),data:params,type:"POST",});return!1};var itemVisibility=function(){var $this=$(this);var $parent=$this.parent();var $list=$this.siblings('.custom_course_menu_list');var hidden=$this.hasClass('inconspicuous');var toAdd="";var toRemove="";if(hidden){toAdd='visible';toRemove='inconspicuous'}else{toAdd='inconspicuous';toRemove='visible'}
$this.removeClass(toRemove).addClass(toAdd);$list.removeClass(toRemove).addClass(toAdd);$parent.removeClass(toRemove).addClass(toAdd);$this.html($('#custom_course_menu_'+toAdd).html());var sessionid=M.cfg.sesskey;var params={sesskey:sessionid,};$.ajax({url:$this.attr('href'),data:params,type:"POST",});return!1};var createInterface=function(editing){var sessionid=M.cfg.sesskey;var params={editing:editing?1:0,sesskey:sessionid,datetime:Date.now(),};$.ajax({url:interfaceUrl,data:params,}).done(function(html){$container.html(html);if(editing){$container.addClass('editing');$('.editingmode').addClass('editingon').removeClass('editingoff')}else{$container.removeClass('editing');$('.editingmode').addClass('editingoff').removeClass('editingon')}
if(html=='You are not enrolled in any courses.'){$('#custom_course_menu_interface').hide()}else{$('#custom_course_menu_interface').show()}
if($('.course-sortable').length){$('.course-sortable').sortable(sortableObject)}
$('.category_switcher').click(categorySwitcher);$('.item_favorite').click(itemFavorite);$('.item_visibility').click(itemVisibility)})};createInterface();var itemFavorite=function(){var $this=$(this);var sessionid=M.cfg.sesskey;var params={sesskey:sessionid,datetime:Date.now(),};$.ajax({url:$this.attr('href'),data:params,type:"POST",}).done(function(){createInterface(!0)});return!1};$('#custom_course_menu_interface').click(function(){var loading=M.util.get_string('loading','block_custom_course_menu');$container.html($('<span/>').addClass('interface').text(loading));createInterface(!$container.hasClass('editing'));return!1});$('#custom_course_menu_interface').mouseenter(function(){if($container.hasClass('editing')){var editingon=M.util.get_string('editingon','block_custom_course_menu');$(this).prepend("<span id='overtext'>"+editingon+"</span>")}else{var editingoff=M.util.get_string('editingoff','block_custom_course_menu');$(this).prepend("<span id='overtext'>"+editingoff+"</span>")}}).mouseleave(function(){$('#overtext').remove()})}))