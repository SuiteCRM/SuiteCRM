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

class GoogleCloudContactcenterinsightsV1QaQuestion extends \Google\Collection
{
  protected $collection_key = 'tags';
  /**
   * @var string
   */
  public $abbreviation;
  protected $answerChoicesType = GoogleCloudContactcenterinsightsV1QaQuestionAnswerChoice::class;
  protected $answerChoicesDataType = 'array';
  /**
   * @var string
   */
  public $answerInstructions;
  /**
   * @var string
   */
  public $createTime;
  protected $metricsType = GoogleCloudContactcenterinsightsV1QaQuestionMetrics::class;
  protected $metricsDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var int
   */
  public $order;
  /**
   * @var string
   */
  public $questionBody;
  /**
   * @var string[]
   */
  public $tags;
  protected $tuningMetadataType = GoogleCloudContactcenterinsightsV1QaQuestionTuningMetadata::class;
  protected $tuningMetadataDataType = '';
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setAbbreviation($abbreviation)
  {
    $this->abbreviation = $abbreviation;
  }
  /**
   * @return string
   */
  public function getAbbreviation()
  {
    return $this->abbreviation;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1QaQuestionAnswerChoice[]
   */
  public function setAnswerChoices($answerChoices)
  {
    $this->answerChoices = $answerChoices;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1QaQuestionAnswerChoice[]
   */
  public function getAnswerChoices()
  {
    return $this->answerChoices;
  }
  /**
   * @param string
   */
  public function setAnswerInstructions($answerInstructions)
  {
    $this->answerInstructions = $answerInstructions;
  }
  /**
   * @return string
   */
  public function getAnswerInstructions()
  {
    return $this->answerInstructions;
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
   * @param GoogleCloudContactcenterinsightsV1QaQuestionMetrics
   */
  public function setMetrics(GoogleCloudContactcenterinsightsV1QaQuestionMetrics $metrics)
  {
    $this->metrics = $metrics;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1QaQuestionMetrics
   */
  public function getMetrics()
  {
    return $this->metrics;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param int
   */
  public function setOrder($order)
  {
    $this->order = $order;
  }
  /**
   * @return int
   */
  public function getOrder()
  {
    return $this->order;
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
  /**
   * @param GoogleCloudContactcenterinsightsV1QaQuestionTuningMetadata
   */
  public function setTuningMetadata(GoogleCloudContactcenterinsightsV1QaQuestionTuningMetadata $tuningMetadata)
  {
    $this->tuningMetadata = $tuningMetadata;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1QaQuestionTuningMetadata
   */
  public function getTuningMetadata()
  {
    return $this->tuningMetadata;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1QaQuestion::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1QaQuestion');
