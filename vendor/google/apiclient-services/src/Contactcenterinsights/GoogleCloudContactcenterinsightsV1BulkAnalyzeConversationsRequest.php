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

class GoogleCloudContactcenterinsightsV1BulkAnalyzeConversationsRequest extends \Google\Model
{
  /**
   * @var float
   */
  public $analysisPercentage;
  protected $annotatorSelectorType = GoogleCloudContactcenterinsightsV1AnnotatorSelector::class;
  protected $annotatorSelectorDataType = '';
  /**
   * @var string
   */
  public $filter;
  /**
   * @var string
   */
  public $parent;

  /**
   * @param float
   */
  public function setAnalysisPercentage($analysisPercentage)
  {
    $this->analysisPercentage = $analysisPercentage;
  }
  /**
   * @return float
   */
  public function getAnalysisPercentage()
  {
    return $this->analysisPercentage;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1AnnotatorSelector
   */
  public function setAnnotatorSelector(GoogleCloudContactcenterinsightsV1AnnotatorSelector $annotatorSelector)
  {
    $this->annotatorSelector = $annotatorSelector;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1AnnotatorSelector
   */
  public function getAnnotatorSelector()
  {
    return $this->annotatorSelector;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1BulkAnalyzeConversationsRequest::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1BulkAnalyzeConversationsRequest');
