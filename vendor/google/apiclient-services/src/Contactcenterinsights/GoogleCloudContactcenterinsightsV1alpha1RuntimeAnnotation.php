<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Contactcenterinsights;

class GoogleCloudContactcenterinsightsV1alpha1RuntimeAnnotation extends \Google\Model
{
  /**
   * @var string
   */
  public $annotationId;
  protected $answerFeedbackType = GoogleCloudContactcenterinsightsV1alpha1AnswerFeedback::class;
  protected $answerFeedbackDataType = '';
  protected $articleSuggestionType = GoogleCloudContactcenterinsightsV1alpha1ArticleSuggestionData::class;
  protected $articleSuggestionDataType = '';
  protected $conversationSummarizationSuggestionType = GoogleCloudContactcenterinsightsV1alpha1ConversationSummarizationSuggestionData::class;
  protected $conversationSummarizationSuggestionDataType = '';
  /**
   * @var string
   */
  public $createTime;
  protected $dialogflowInteractionType = GoogleCloudContactcenterinsightsV1alpha1DialogflowInteractionData::class;
  protected $dialogflowInteractionDataType = '';
  protected $endBoundaryType = GoogleCloudContactcenterinsightsV1alpha1AnnotationBoundary::class;
  protected $endBoundaryDataType = '';
  protected $faqAnswerType = GoogleCloudContactcenterinsightsV1alpha1FaqAnswerData::class;
  protected $faqAnswerDataType = '';
  protected $smartComposeSuggestionType = GoogleCloudContactcenterinsightsV1alpha1SmartComposeSuggestionData::class;
  protected $smartComposeSuggestionDataType = '';
  protected $smartReplyType = GoogleCloudContactcenterinsightsV1alpha1SmartReplyData::class;
  protected $smartReplyDataType = '';
  protected $startBoundaryType = GoogleCloudContactcenterinsightsV1alpha1AnnotationBoundary::class;
  protected $startBoundaryDataType = '';
  protected $userInputType = GoogleCloudContactcenterinsightsV1alpha1RuntimeAnnotationUserInput::class;
  protected $userInputDataType = '';

  /**
   * @param string
   */
  public function setAnnotationId($annotationId)
  {
    $this->annotationId = $annotationId;
  }
  /**
   * @return string
   */
  public function getAnnotationId()
  {
    return $this->annotationId;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1AnswerFeedback
   */
  public function setAnswerFeedback(GoogleCloudContactcenterinsightsV1alpha1AnswerFeedback $answerFeedback)
  {
    $this->answerFeedback = $answerFeedback;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1AnswerFeedback
   */
  public function getAnswerFeedback()
  {
    return $this->answerFeedback;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1ArticleSuggestionData
   */
  public function setArticleSuggestion(GoogleCloudContactcenterinsightsV1alpha1ArticleSuggestionData $articleSuggestion)
  {
    $this->articleSuggestion = $articleSuggestion;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1ArticleSuggestionData
   */
  public function getArticleSuggestion()
  {
    return $this->articleSuggestion;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1ConversationSummarizationSuggestionData
   */
  public function setConversationSummarizationSuggestion(GoogleCloudContactcenterinsightsV1alpha1ConversationSummarizationSuggestionData $conversationSummarizationSuggestion)
  {
    $this->conversationSummarizationSuggestion = $conversationSummarizationSuggestion;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1ConversationSummarizationSuggestionData
   */
  public function getConversationSummarizationSuggestion()
  {
    return $this->conversationSummarizationSuggestion;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1DialogflowInteractionData
   */
  public function setDialogflowInteraction(GoogleCloudContactcenterinsightsV1alpha1DialogflowInteractionData $dialogflowInteraction)
  {
    $this->dialogflowInteraction = $dialogflowInteraction;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1DialogflowInteractionData
   */
  public function getDialogflowInteraction()
  {
    return $this->dialogflowInteraction;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1AnnotationBoundary
   */
  public function setEndBoundary(GoogleCloudContactcenterinsightsV1alpha1AnnotationBoundary $endBoundary)
  {
    $this->endBoundary = $endBoundary;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1AnnotationBoundary
   */
  public function getEndBoundary()
  {
    return $this->endBoundary;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1FaqAnswerData
   */
  public function setFaqAnswer(GoogleCloudContactcenterinsightsV1alpha1FaqAnswerData $faqAnswer)
  {
    $this->faqAnswer = $faqAnswer;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1FaqAnswerData
   */
  public function getFaqAnswer()
  {
    return $this->faqAnswer;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1SmartComposeSuggestionData
   */
  public function setSmartComposeSuggestion(GoogleCloudContactcenterinsightsV1alpha1SmartComposeSuggestionData $smartComposeSuggestion)
  {
    $this->smartComposeSuggestion = $smartComposeSuggestion;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1SmartComposeSuggestionData
   */
  public function getSmartComposeSuggestion()
  {
    return $this->smartComposeSuggestion;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1SmartReplyData
   */
  public function setSmartReply(GoogleCloudContactcenterinsightsV1alpha1SmartReplyData $smartReply)
  {
    $this->smartReply = $smartReply;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1SmartReplyData
   */
  public function getSmartReply()
  {
    return $this->smartReply;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1AnnotationBoundary
   */
  public function setStartBoundary(GoogleCloudContactcenterinsightsV1alpha1AnnotationBoundary $startBoundary)
  {
    $this->startBoundary = $startBoundary;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1AnnotationBoundary
   */
  public function getStartBoundary()
  {
    return $this->startBoundary;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1RuntimeAnnotationUserInput
   */
  public function setUserInput(GoogleCloudContactcenterinsightsV1alpha1RuntimeAnnotationUserInput $userInput)
  {
    $this->userInput = $userInput;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1RuntimeAnnotationUserInput
   */
  public function getUserInput()
  {
    return $this->userInput;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1alpha1RuntimeAnnotation::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1alpha1RuntimeAnnotation');
