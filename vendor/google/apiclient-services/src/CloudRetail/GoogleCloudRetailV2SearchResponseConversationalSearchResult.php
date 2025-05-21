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

namespace Google\Service\CloudRetail;

class GoogleCloudRetailV2SearchResponseConversationalSearchResult extends \Google\Collection
{
  protected $collection_key = 'suggestedAnswers';
  protected $additionalFilterType = GoogleCloudRetailV2SearchResponseConversationalSearchResultAdditionalFilter::class;
  protected $additionalFilterDataType = '';
  protected $additionalFiltersType = GoogleCloudRetailV2SearchResponseConversationalSearchResultAdditionalFilter::class;
  protected $additionalFiltersDataType = 'array';
  /**
   * @var string
   */
  public $conversationId;
  /**
   * @var string
   */
  public $followupQuestion;
  /**
   * @var string
   */
  public $refinedQuery;
  protected $suggestedAnswersType = GoogleCloudRetailV2SearchResponseConversationalSearchResultSuggestedAnswer::class;
  protected $suggestedAnswersDataType = 'array';

  /**
   * @param GoogleCloudRetailV2SearchResponseConversationalSearchResultAdditionalFilter
   */
  public function setAdditionalFilter(GoogleCloudRetailV2SearchResponseConversationalSearchResultAdditionalFilter $additionalFilter)
  {
    $this->additionalFilter = $additionalFilter;
  }
  /**
   * @return GoogleCloudRetailV2SearchResponseConversationalSearchResultAdditionalFilter
   */
  public function getAdditionalFilter()
  {
    return $this->additionalFilter;
  }
  /**
   * @param GoogleCloudRetailV2SearchResponseConversationalSearchResultAdditionalFilter[]
   */
  public function setAdditionalFilters($additionalFilters)
  {
    $this->additionalFilters = $additionalFilters;
  }
  /**
   * @return GoogleCloudRetailV2SearchResponseConversationalSearchResultAdditionalFilter[]
   */
  public function getAdditionalFilters()
  {
    return $this->additionalFilters;
  }
  /**
   * @param string
   */
  public function setConversationId($conversationId)
  {
    $this->conversationId = $conversationId;
  }
  /**
   * @return string
   */
  public function getConversationId()
  {
    return $this->conversationId;
  }
  /**
   * @param string
   */
  public function setFollowupQuestion($followupQuestion)
  {
    $this->followupQuestion = $followupQuestion;
  }
  /**
   * @return string
   */
  public function getFollowupQuestion()
  {
    return $this->followupQuestion;
  }
  /**
   * @param string
   */
  public function setRefinedQuery($refinedQuery)
  {
    $this->refinedQuery = $refinedQuery;
  }
  /**
   * @return string
   */
  public function getRefinedQuery()
  {
    return $this->refinedQuery;
  }
  /**
   * @param GoogleCloudRetailV2SearchResponseConversationalSearchResultSuggestedAnswer[]
   */
  public function setSuggestedAnswers($suggestedAnswers)
  {
    $this->suggestedAnswers = $suggestedAnswers;
  }
  /**
   * @return GoogleCloudRetailV2SearchResponseConversationalSearchResultSuggestedAnswer[]
   */
  public function getSuggestedAnswers()
  {
    return $this->suggestedAnswers;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRetailV2SearchResponseConversationalSearchResult::class, 'Google_Service_CloudRetail_GoogleCloudRetailV2SearchResponseConversationalSearchResult');
