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

class GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsRequest extends \Google\Collection
{
  protected $collection_key = 'templateQaScorecardId';
  /**
   * @var string
   */
  public $conversationFilter;
  /**
   * @var string
   */
  public $feedbackLabelType;
  /**
   * @var string
   */
  public $filter;
  protected $gcsDestinationType = GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsRequestGcsDestination::class;
  protected $gcsDestinationDataType = '';
  /**
   * @var int
   */
  public $maxDownloadCount;
  /**
   * @var string
   */
  public $parent;
  /**
   * @var string[]
   */
  public $templateQaScorecardId;

  /**
   * @param string
   */
  public function setConversationFilter($conversationFilter)
  {
    $this->conversationFilter = $conversationFilter;
  }
  /**
   * @return string
   */
  public function getConversationFilter()
  {
    return $this->conversationFilter;
  }
  /**
   * @param string
   */
  public function setFeedbackLabelType($feedbackLabelType)
  {
    $this->feedbackLabelType = $feedbackLabelType;
  }
  /**
   * @return string
   */
  public function getFeedbackLabelType()
  {
    return $this->feedbackLabelType;
  }
  /**
   * @param string
   */
  public function setFilter($filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return string
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsRequestGcsDestination
   */
  public function setGcsDestination(GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsRequestGcsDestination $gcsDestination)
  {
    $this->gcsDestination = $gcsDestination;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsRequestGcsDestination
   */
  public function getGcsDestination()
  {
    return $this->gcsDestination;
  }
  /**
   * @param int
   */
  public function setMaxDownloadCount($maxDownloadCount)
  {
    $this->maxDownloadCount = $maxDownloadCount;
  }
  /**
   * @return int
   */
  public function getMaxDownloadCount()
  {
    return $this->maxDownloadCount;
  }
  /**
   * @param string
   */
  public function setParent($parent)
  {
    $this->parent = $parent;
  }
  /**
   * @return string
   */
  public function getParent()
  {
    return $this->parent;
  }
  /**
   * @param string[]
   */
  public function setTemplateQaScorecardId($templateQaScorecardId)
  {
    $this->templateQaScorecardId = $templateQaScorecardId;
  }
  /**
   * @return string[]
   */
  public function getTemplateQaScorecardId()
  {
    return $this->templateQaScorecardId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsRequest::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsRequest');
