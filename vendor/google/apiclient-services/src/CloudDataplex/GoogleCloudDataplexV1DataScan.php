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

class GoogleCloudDataplexV1DataScan extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  protected $dataType = GoogleCloudDataplexV1DataSource::class;
  protected $dataDataType = '';
  protected $dataDiscoveryResultType = GoogleCloudDataplexV1DataDiscoveryResult::class;
  protected $dataDiscoveryResultDataType = '';
  protected $dataDiscoverySpecType = GoogleCloudDataplexV1DataDiscoverySpec::class;
  protected $dataDiscoverySpecDataType = '';
  protected $dataProfileResultType = GoogleCloudDataplexV1DataProfileResult::class;
  protected $dataProfileResultDataType = '';
  protected $dataProfileSpecType = GoogleCloudDataplexV1DataProfileSpec::class;
  protected $dataProfileSpecDataType = '';
  protected $dataQualityResultType = GoogleCloudDataplexV1DataQualityResult::class;
  protected $dataQualityResultDataType = '';
  protected $dataQualitySpecType = GoogleCloudDataplexV1DataQualitySpec::class;
  protected $dataQualitySpecDataType = '';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  protected $executionSpecType = GoogleCloudDataplexV1DataScanExecutionSpec::class;
  protected $executionSpecDataType = '';
  protected $executionStatusType = GoogleCloudDataplexV1DataScanExecutionStatus::class;
  protected $executionStatusDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;

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
   * @param GoogleCloudDataplexV1DataSource
   */
  public function setData(GoogleCloudDataplexV1DataSource $data)
  {
    $this->data = $data;
  }
  /**
   * @return GoogleCloudDataplexV1DataSource
   */
  public function getData()
  {
    return $this->data;
  }
  /**
   * @param GoogleCloudDataplexV1DataDiscoveryResult
   */
  public function setDataDiscoveryResult(GoogleCloudDataplexV1DataDiscoveryResult $dataDiscoveryResult)
  {
    $this->dataDiscoveryResult = $dataDiscoveryResult;
  }
  /**
   * @return GoogleCloudDataplexV1DataDiscoveryResult
   */
  public function getDataDiscoveryResult()
  {
    return $this->dataDiscoveryResult;
  }
  /**
   * @param GoogleCloudDataplexV1DataDiscoverySpec
   */
  public function setDataDiscoverySpec(GoogleCloudDataplexV1DataDiscoverySpec $dataDiscoverySpec)
  {
    $this->dataDiscoverySpec = $dataDiscoverySpec;
  }
  /**
   * @return GoogleCloudDataplexV1DataDiscoverySpec
   */
  public function getDataDiscoverySpec()
  {
    return $this->dataDiscoverySpec;
  }
  /**
   * @param GoogleCloudDataplexV1DataProfileResult
   */
  public function setDataProfileResult(GoogleCloudDataplexV1DataProfileResult $dataProfileResult)
  {
    $this->dataProfileResult = $dataProfileResult;
  }
  /**
   * @return GoogleCloudDataplexV1DataProfileResult
   */
  public function getDataProfileResult()
  {
    return $this->dataProfileResult;
  }
  /**
   * @param GoogleCloudDataplexV1DataProfileSpec
   */
  public function setDataProfileSpec(GoogleCloudDataplexV1DataProfileSpec $dataProfileSpec)
  {
    $this->dataProfileSpec = $dataProfileSpec;
  }
  /**
   * @return GoogleCloudDataplexV1DataProfileSpec
   */
  public function getDataProfileSpec()
  {
    return $this->dataProfileSpec;
  }
  /**
   * @param GoogleCloudDataplexV1DataQualityResult
   */
  public function setDataQualityResult(GoogleCloudDataplexV1DataQualityResult $dataQualityResult)
  {
    $this->dataQualityResult = $dataQualityResult;
  }
  /**
   * @return GoogleCloudDataplexV1DataQualityResult
   */
  public function getDataQualityResult()
  {
    return $this->dataQualityResult;
  }
  /**
   * @param GoogleCloudDataplexV1DataQualitySpec
   */
  public function setDataQualitySpec(GoogleCloudDataplexV1DataQualitySpec $dataQualitySpec)
  {
    $this->dataQualitySpec = $dataQualitySpec;
  }
  /**
   * @return GoogleCloudDataplexV1DataQualitySpec
   */
  public function getDataQualitySpec()
  {
    return $this->dataQualitySpec;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param GoogleCloudDataplexV1DataScanExecutionSpec
   */
  public function setExecutionSpec(GoogleCloudDataplexV1DataScanExecutionSpec $executionSpec)
  {
    $this->executionSpec = $executionSpec;
  }
  /**
   * @return GoogleCloudDataplexV1DataScanExecutionSpec
   */
  public function getExecutionSpec()
  {
    return $this->executionSpec;
  }
  /**
   * @param GoogleCloudDataplexV1DataScanExecutionStatus
   */
  public function setExecutionStatus(GoogleCloudDataplexV1DataScanExecutionStatus $executionStatus)
  {
    $this->executionStatus = $executionStatus;
  }
  /**
   * @return GoogleCloudDataplexV1DataScanExecutionStatus
   */
  public function getExecutionStatus()
  {
    return $this->executionStatus;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param string
   */
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
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
class_alias(GoogleCloudDataplexV1DataScan::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataScan');
