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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1DataQualitySpec extends \Google\Collection
{
  protected $collection_key = 'rules';
  protected $postScanActionsType = GoogleCloudDataplexV1DataQualitySpecPostScanActions::class;
  protected $postScanActionsDataType = '';
  /**
   * @var string
   */
  public $rowFilter;
  protected $rulesType = GoogleCloudDataplexV1DataQualityRule::class;
  protected $rulesDataType = 'array';
  /**
   * @var float
   */
  public $samplingPercent;

  /**
   * @param GoogleCloudDataplexV1DataQualitySpecPostScanActions
   */
  public function setPostScanActions(GoogleCloudDataplexV1DataQualitySpecPostScanActions $postScanActions)
  {
    $this->postScanActions = $postScanActions;
  }
  /**
   * @return GoogleCloudDataplexV1DataQualitySpecPostScanActions
   */
  public function getPostScanActions()
  {
    return $this->postScanActions;
  }
  /**
   * @param string
   */
  public function setRowFilter($rowFilter)
  {
    $this->rowFilter = $rowFilter;
  }
  /**
   * @return string
   */
  public function getRowFilter()
  {
    return $this->rowFilter;
  }
  /**
   * @param GoogleCloudDataplexV1DataQualityRule[]
   */
  public function setRules($rules)
  {
    $this->rules = $rules;
  }
  /**
   * @return GoogleCloudDataplexV1DataQualityRule[]
   */
  public function getRules()
  {
    return $this->rules;
  }
  /**
   * @param float
   */
  public function setSamplingPercent($samplingPercent)
  {
    $this->samplingPercent = $samplingPercent;
  }
  /**
   * @return float
   */
  public function getSamplingPercent()
  {
    return $this->samplingPercent;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataQualitySpec::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataQualitySpec');
