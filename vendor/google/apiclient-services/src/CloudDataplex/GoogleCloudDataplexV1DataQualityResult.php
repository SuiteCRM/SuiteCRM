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

class GoogleCloudDataplexV1DataQualityResult extends \Google\Collection
{
  protected $collection_key = 'rules';
  protected $columnsType = GoogleCloudDataplexV1DataQualityColumnResult::class;
  protected $columnsDataType = 'array';
  protected $dimensionsType = GoogleCloudDataplexV1DataQualityDimensionResult::class;
  protected $dimensionsDataType = 'array';
  /**
   * @var bool
   */
  public $passed;
  protected $postScanActionsResultType = GoogleCloudDataplexV1DataQualityResultPostScanActionsResult::class;
  protected $postScanActionsResultDataType = '';
  /**
   * @var string
   */
  public $rowCount;
  protected $rulesType = GoogleCloudDataplexV1DataQualityRuleResult::class;
  protected $rulesDataType = 'array';
  protected $scannedDataType = GoogleCloudDataplexV1ScannedData::class;
  protected $scannedDataDataType = '';
  /**
   * @var float
   */
  public $score;

  /**
   * @param GoogleCloudDataplexV1DataQualityColumnResult[]
   */
  public function setColumns($columns)
  {
    $this->columns = $columns;
  }
  /**
   * @return GoogleCloudDataplexV1DataQualityColumnResult[]
   */
  public function getColumns()
  {
    return $this->columns;
  }
  /**
   * @param GoogleCloudDataplexV1DataQualityDimensionResult[]
   */
  public function setDimensions($dimensions)
  {
    $this->dimensions = $dimensions;
  }
  /**
   * @return GoogleCloudDataplexV1DataQualityDimensionResult[]
   */
  public function getDimensions()
  {
    return $this->dimensions;
  }
  /**
   * @param bool
   */
  public function setPassed($passed)
  {
    $this->passed = $passed;
  }
  /**
   * @return bool
   */
  public function getPassed()
  {
    return $this->passed;
  }
  /**
   * @param GoogleCloudDataplexV1DataQualityResultPostScanActionsResult
   */
  public function setPostScanActionsResult(GoogleCloudDataplexV1DataQualityResultPostScanActionsResult $postScanActionsResult)
  {
    $this->postScanActionsResult = $postScanActionsResult;
  }
  /**
   * @return GoogleCloudDataplexV1DataQualityResultPostScanActionsResult
   */
  public function getPostScanActionsResult()
  {
    return $this->postScanActionsResult;
  }
  /**
   * @param string
   */
  public function setRowCount($rowCount)
  {
    $this->rowCount = $rowCount;
  }
  /**
   * @return string
   */
  public function getRowCount()
  {
    return $this->rowCount;
  }
  /**
   * @param GoogleCloudDataplexV1DataQualityRuleResult[]
   */
  public function setRules($rules)
  {
    $this->rules = $rules;
  }
  /**
   * @return GoogleCloudDataplexV1DataQualityRuleResult[]
   */
  public function getRules()
  {
    return $this->rules;
  }
  /**
   * @param GoogleCloudDataplexV1ScannedData
   */
  public function setScannedData(GoogleCloudDataplexV1ScannedData $scannedData)
  {
    $this->scannedData = $scannedData;
  }
  /**
   * @return GoogleCloudDataplexV1ScannedData
   */
  public function getScannedData()
  {
    return $this->scannedData;
  }
  /**
   * @param float
   */
  public function setScore($score)
  {
    $this->score = $score;
  }
  /**
   * @return float
   */
  public function getScore()
  {
    return $this->score;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataQualityResult::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataQualityResult');
