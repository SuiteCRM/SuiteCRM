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

class GoogleCloudDataplexV1MetadataJob extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  protected $importResultType = GoogleCloudDataplexV1MetadataJobImportJobResult::class;
  protected $importResultDataType = '';
  protected $importSpecType = GoogleCloudDataplexV1MetadataJobImportJobSpec::class;
  protected $importSpecDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  protected $statusType = GoogleCloudDataplexV1MetadataJobStatus::class;
  protected $statusDataType = '';
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
   * @param GoogleCloudDataplexV1MetadataJobImportJobResult
   */
  public function setImportResult(GoogleCloudDataplexV1MetadataJobImportJobResult $importResult)
  {
    $this->importResult = $importResult;
  }
  /**
   * @return GoogleCloudDataplexV1MetadataJobImportJobResult
   */
  public function getImportResult()
  {
    return $this->importResult;
  }
  /**
   * @param GoogleCloudDataplexV1MetadataJobImportJobSpec
   */
  public function setImportSpec(GoogleCloudDataplexV1MetadataJobImportJobSpec $importSpec)
  {
    $this->importSpec = $importSpec;
  }
  /**
   * @return GoogleCloudDataplexV1MetadataJobImportJobSpec
   */
  public function getImportSpec()
  {
    return $this->importSpec;
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
   * @param GoogleCloudDataplexV1MetadataJobStatus
   */
  public function setStatus(GoogleCloudDataplexV1MetadataJobStatus $status)
  {
    $this->status = $status;
  }
  /**
   * @return GoogleCloudDataplexV1MetadataJobStatus
   */
  public function getStatus()
  {
    return $this->status;
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
class_alias(GoogleCloudDataplexV1MetadataJob::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1MetadataJob');
