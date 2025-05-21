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

class GoogleCloudDataplexV1DataProfileSpec extends \Google\Model
{
  protected $excludeFieldsType = GoogleCloudDataplexV1DataProfileSpecSelectedFields::class;
  protected $excludeFieldsDataType = '';
  protected $includeFieldsType = GoogleCloudDataplexV1DataProfileSpecSelectedFields::class;
  protected $includeFieldsDataType = '';
  protected $postScanActionsType = GoogleCloudDataplexV1DataProfileSpecPostScanActions::class;
  protected $postScanActionsDataType = '';
  /**
   * @var string
   */
  public $rowFilter;
  /**
   * @var float
   */
  public $samplingPercent;

  /**
   * @param GoogleCloudDataplexV1DataProfileSpecSelectedFields
   */
  public function setExcludeFields(GoogleCloudDataplexV1DataProfileSpecSelectedFields $excludeFields)
  {
    $this->excludeFields = $excludeFields;
  }
  /**
   * @return GoogleCloudDataplexV1DataProfileSpecSelectedFields
   */
  public function getExcludeFields()
  {
    return $this->excludeFields;
  }
  /**
   * @param GoogleCloudDataplexV1DataProfileSpecSelectedFields
   */
  public function setIncludeFields(GoogleCloudDataplexV1DataProfileSpecSelectedFields $includeFields)
  {
    $this->includeFields = $includeFields;
  }
  /**
   * @return GoogleCloudDataplexV1DataProfileSpecSelectedFields
   */
  public function getIncludeFields()
  {
    return $this->includeFields;
  }
  /**
   * @param GoogleCloudDataplexV1DataProfileSpecPostScanActions
   */
  public function setPostScanActions(GoogleCloudDataplexV1DataProfileSpecPostScanActions $postScanActions)
  {
    $this->postScanActions = $postScanActions;
  }
  /**
   * @return GoogleCloudDataplexV1DataProfileSpecPostScanActions
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
class_alias(GoogleCloudDataplexV1DataProfileSpec::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataProfileSpec');
