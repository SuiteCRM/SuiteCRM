<div>
    <input type="hidden" name="survey_questions_supplied" value="1">
    <table id="questionTable" class="table table-bordered">
        <tr>
          <th>
            {$MOD.LBL_SURVEY_QUESTION}
          </th>
          <th>
            {$MOD.LBL_SURVEY_TEXT}
          </th>
          <th>
            {$MOD.LBL_SURVEY_TYPE}
          </th>
            <th>{$MOD.LBL_SURVEY_ACTIONS}</th>
        </tr>
    </table>
    <button type="button" class="button" id="newQuestionButton">{$MOD.LBL_SURVEY_NEW_QUESTION}</button>
</div>
{literal}
<script>
  $(document).ready(function () {

    function updateType(ev) {
      var target = $(ev.target);
      var options = target.closest('tbody').find('.questionOptions');

      switch (target.val()) {
        case "Radio":
        case "Dropdown":
        case "Multiselect":
        case "Matrix":
          options.show();
          break;
        case "Rating":
        case "Scale":
        case "DateTime":
        case "Date":
        case "Textbox":
        case "Checkbox":
        case "Text":
        default:
          options.hide();
          break;
      }
    }

    function addOption(ev, data) {
      var target = $(ev.target);
      var questionIndex = target.data('question-index');
      var list = target.closest('td').find('ul');
      var html = "<li>"
        + "<input type='hidden' class='survey_question_options_id' name='survey_questions_options_id[" + questionIndex + "][]'/>"
        + "<input type='hidden' class='survey_question_options_deleted' name='survey_questions_options_deleted[" + questionIndex + "][]' value='0'/>"
        + "<input class='survey_question_options' name='survey_questions_options[" + questionIndex + "][]' type='text'/>"
        + "<button type='button' class='button deleteQuestionOption'><span class='suitepicon suitepicon-action-clear'></span></button>"
        + "</li>";
      var item = $(html);
      if (data) {
        item.find('.survey_question_options').val(data.name);
        item.find('.survey_question_options_id').val(data.id);
      }
      item.find('.deleteQuestionOption').click(deleteQuestionOption);
      list.append(item);
    }

    function deleteQuestionOption(e) {
      var target = $(e.target);
      var li = target.closest('li');
      li.hide();
      li.find('.survey_question_options_deleted').val(1);
    }

    function moveQuestionUp(e) {
      var target = $(e.target);
      var tbody = target.closest('tbody');
      tbody.insertBefore(tbody.prev('.questionBody'));
      fixSortOrders();
    }

    function moveQuestionDown(e) {
      var target = $(e.target);
      var tbody = target.closest('tbody');
      tbody.insertAfter(tbody.next());
      fixSortOrders();
    }

    function deleteQuestion(e) {
      var target = $(e.target);
      var tbody = target.closest('tbody');
      tbody.find('.surveyQuestionDeleted').val(1);
      tbody.hide();
      fixSortOrders();
    }

    function fixSortOrders() {
      var count = 0;
      $('.surveyQuestionSortOrder').each(function (ind, e) {
        var elem = $(e);
        elem.val(count);
        var deleted = elem.siblings('.surveyQuestionDeleted').get(0).value;
        if (deleted == 1) {
          return;
        }

        elem.closest('tbody').find('.nameSpan').text("Q" + (count + 1));
        count++;
      });
    }

    function createQuestion(data, existing) {

      var table = $('#questionTable');
      var newRow = "<tbody class='questionBody'><tr data-question-index='" + createQuestion.questionCount + "'>";
      newRow += "<td class='nameCell'><span class='nameSpan'>Q" + (createQuestion.questionCount + 1) + "</span>"
        + "<input type='hidden' class='surveyQuestionId' name='survey_questions_ids[" + createQuestion.questionCount + "]'> "
        + "<input type='hidden' class='surveyQuestionDeleted' name='survey_questions_deleted[" + createQuestion.questionCount + "]' value='0'> "
        + "<input type='hidden' class='surveyQuestionSortOrder' name='survey_questions_sortorder[" + createQuestion.questionCount + "]' value='" + createQuestion.questionCount + "'> "
        + "</td>";
      newRow += "<td><input class='surveyQuestionName' name='survey_questions_names[" + createQuestion.questionCount + "]' type='text'></td>";
      newRow += "<td><select class='surveyQuestionType' name='survey_questions_types[" + createQuestion.questionCount + "]'>{/literal}{$question_type_options}{literal}</select></td>";
      newRow += "<td>";

      newRow += "<button type='button' class='button moveQuestionUp'><span class='suitepicon suitepicon-action-move-up'></span></button>";
      newRow += "<button type='button' class='button moveQuestionDown'><span class='suitepicon suitepicon-action-move-down'></span></button>";
      newRow += "&nbsp;&nbsp;&nbsp;";
      newRow += "<button type='button' class='button deleteQuestion'><span class='suitepicon suitepicon-action-clear'></span></button>";
      newRow += "</td>";
      newRow += "</tr>";
      newRow += "<tr class='questionOptions' style='display: none;'>";
      newRow += "<td>";
      newRow += "</td>";
      newRow += "<td>";
      newRow += "</td>";
      newRow += "<td>";
      newRow += "<strong>{/literal}{$MOD.LBL_OPTIONS}{literal}</strong>";
      newRow += "<ul style='list-style: none;' class='optionList'>";
      newRow += "</ul>";
      newRow += "<button class='addOptionButton button' data-question-index='" + createQuestion.questionCount + "' type='button'>{/literal}{$MOD.LBL_ADD_OPTION}{literal}</button>";
      newRow += "</td>";
      newRow += "<td>";
      newRow += "</td>";
      newRow += "</tr>";
      newRow += "</tbody>";
      newRow = $(newRow);
      if (existing) {
        newRow.find('.surveyQuestionName').val(data.name);
        newRow.find('.surveyQuestionType').val(data.type);
        newRow.find('.surveyQuestionId').val(data.id);
      }
      newRow.find('.addOptionButton').click(addOption);
      if (typeof data.options !== 'undefined') {
        for (var x = 0; x < data.options.length; x++) {
          addOption({target: newRow.find('.addOptionButton')}, data.options[x]);
        }
      }
      newRow.find('.surveyQuestionType').change(updateType);
      newRow.find('.moveQuestionUp').click(moveQuestionUp);
      newRow.find('.moveQuestionDown').click(moveQuestionDown);
      newRow.find('.deleteQuestion').click(deleteQuestion);
      //Add to DOM first so we trigger any external change listeners.
      table.append(newRow);
      newRow.find('.surveyQuestionType').change();

      createQuestion.questionCount++;
      fixSortOrders();
    }

    createQuestion.questionCount = 0;
      {/literal}
      {foreach from=$questions item=question}
    createQuestion({$question|@json_encode}, true);
      {/foreach}
      {literal}
    $('#newQuestionButton').click(createQuestion);
  });

</script>
{/literal}
