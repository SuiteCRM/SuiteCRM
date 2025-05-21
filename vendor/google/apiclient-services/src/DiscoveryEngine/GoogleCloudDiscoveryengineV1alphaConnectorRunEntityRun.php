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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1alphaConnectorRunEntityRun extends \Google\Collection
{
  protected $collection_key = 'errors';
  /**
   * @var string
   */
  public $deletedRecordCount;
  /**
   * @var string
   */
  public $entityName;
  /**
   * @var string
   */
  public $errorRecordCount;
  protected $errorsType = GoogleRpcStatus::class;
  protected $errorsDataType = 'array';
  /**
   * @var string
   */
  public $extractedRecordCount;
  /**
   * @var string
   */
  public $indexedRecordCount;
  /**
   * @var string
   */
  public $sourceApiRequestCount;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $stateUpdateTime;
  /**
   * @var string
   */
  public $statsUpdateTime;
  /**
   * @var string
   */
  public $syncType;

  /**
   * @param string
   */
  public function setDeletedRecordCount($deletedRecordCount)
  {
    $this->deletedRecordCount = $deletedRecordCount;
  }
  /**
   * @return string
   */
  public function getDeletedRecordCount()
  {
    return $this->deletedRecordCount;
  }
  /**
   * @param string
   */
  public function setEntityName($entityName)
  {
    $this->entityName = $entityName;
  }
  /**
   * @return string
   */
  public function getEntityName()
  {
    return $this->entityName;
  }
  /**
   * @param string
   */
  public function setErrorRecordCount($errorRecordCount)
  {
    $this->errorRecordCount = $errorRecordCount;
  }
  /**
   * @return string
   */
  public function getErrorRecordCount()
  {
    return $this->errorRecordCount;
  }
  /**
   * @param GoogleRpcStatus[]
   */
  public function setErrors($errors)
  {
    $this->errors = $errors;
  }
  /**
   * @return GoogleRpcStatus[]
   */
  public function getErrors()
  {
    return $this->errors;
  }
  /**
   * @param string
   */
  public function setExtractedRecordCount($extractedRecordCount)
  {
    $this->extractedRecordCount = $extractedRecordCount;
  }
  /**
   * @return string
   */
  public function getExtractedRecordCount()
  {
    return $this->extractedRecordCount;
  }
  /**
   * @param string
   */
  public function setIndexedRecordCount($indexedRecordCount)
  {
    $this->indexedRecordCount = $indexedRecordCount;
  }
  /**
   * @return string
   */
  public function getIndexedRecordCount()
  {
    return $this->indexedRecordCount;
  }
  /**
   * @param string
   */
  public function setSourceApiRequestCount($sourceApiRequestCount)
  {
    $this->sourceApiRequestCount = $sourceApiRequestCount;
  }
  /**
   * @return string
   */
  public function getSourceApiRequestCount()
  {
    return $this->sourceApiRequestCount;
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
  public function setStateUpdateTime($stateUpdateTime)
  {
    $this->stateUpdateTime = $stateUpdateTime;
  }
  /**
   * @return string
   */
  public function getStateUpdateTime()
  {
    return $this->stateUpdateTime;
  }
  /**
   * @param string
   */
  public function setStatsUpdateTime($statsUpdateTime)
  {
    $this->statsUpdateTime = $statsUpdateTime;
  }
  /**
   * @return string
   */
  public function getStatsUpdateTime()
  {
    return $this->statsUpdateTime;
  }
  /**
   * @param string
   */
  public function setSyncType($syncType)
  {
    $this->syncType = $syncType;
  }
  /**
   * @return string
   */
  public function getSyncType()
  {
    return $this->syncType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaConnectorRunEntityRun::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaConnectorRunEntityRun');
