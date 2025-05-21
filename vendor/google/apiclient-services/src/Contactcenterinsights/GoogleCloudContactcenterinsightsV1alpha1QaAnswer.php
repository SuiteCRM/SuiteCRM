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

class GoogleCloudContactcenterinsightsV1alpha1QaAnswer extends \Google\Collection
{
  protected $collection_key = 'tags';
  protected $answerSourcesType = GoogleCloudContactcenterinsightsV1alpha1QaAnswerAnswerSource::class;
  protected $answerSourcesDataType = 'array';
  protected $answerValueType = GoogleCloudContactcenterinsightsV1alpha1QaAnswerAnswerValue::class;
  protected $answerValueDataType = '';
  /**
   * @var string
   */
  public $conversation;
  /**
   * @var string
   */
  public $qaQuestion;
  /**
   * @var string
   */
  public $questionBody;
  /**
   * @var string[]
   */
  public $tags;

  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1QaAnswerAnswerSource[]
   */
  public function setAnswerSources($answerSources)
  {
    $this->answerSources = $answerSources;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1QaAnswerAnswerSource[]
   */
  public function getAnswerSources()
  {
    return $this->answerSources;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1QaAnswerAnswerValue
   */
  public function setAnswerValue(GoogleCloudContactcenterinsightsV1alpha1QaAnswerAnswerValue $answerValue)
  {
    $this->answerValue = $answerValue;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1QaAnswerAnswerValue
   */
  public function getAnswerValue()
  {
    return $this->answerValue;
  }
  /**
   * @param string
   */
  public function setConversation($conversation)
  {
    $this->conversation = $conversation;
  }
  /**
   * @return string
   */
  public function getConversation()
  {
    return $this->conversation;
  }
  /**
   * @param string
   */
  public function setQaQuestion($qaQuestion)
  {
    $this->qaQuestion = $qaQuestion;
  }
  /**
   * @return string
   */
  public function getQaQuestion()
  {
    return $this->qaQuestion;
  }
  /**
   * @param string
   */
  public function setQuestionBody($questionBody)
  {
    $this->questionBody = $questionBody;
  }
  /**
   * @return string
   */
  public function getQuestionBody()
  {
    return $this->questionBody;
  }
  /**
   * @param string[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return string[]
   */
  public function getTags()
  {
    return $this->tags;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1alpha1QaAnswer::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1alpha1QaAnswer');
