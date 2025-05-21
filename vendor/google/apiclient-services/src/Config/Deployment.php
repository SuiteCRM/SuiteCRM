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

namespace Google\Service\Config;

class Deployment extends \Google\Collection
{
  protected $collection_key = 'tfErrors';
  /**
   * @var string[]
   */
  public $annotations;
  /**
   * @var string
   */
  public $artifactsGcsBucket;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $deleteBuild;
  /**
   * @var string
   */
  public $deleteLogs;
  protected $deleteResultsType = ApplyResults::class;
  protected $deleteResultsDataType = '';
  /**
   * @var string
   */
  public $errorCode;
  /**
   * @var string
   */
  public $errorLogs;
  /**
   * @var bool
   */
  public $importExistingResources;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $latestRevision;
  /**
   * @var string
   */
  public $lockState;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $quotaValidation;
  /**
   * @var string
   */
  public $serviceAccount;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $stateDetail;
  protected $terraformBlueprintType = TerraformBlueprint::class;
  protected $terraformBlueprintDataType = '';
  protected $tfErrorsType = TerraformError::class;
  protected $tfErrorsDataType = 'array';
  /**
   * @var string
   */
  public $tfVersion;
  /**
   * @var string
   */
  public $tfVersionConstraint;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $workerPool;

  /**
   * @param string[]
   */
  public function setAnnotations($annotations)
  {
    $this->annotations = $annotations;
  }
  /**
   * @return string[]
   */
  public function getAnnotations()
  {
    return $this->annotations;
  }
  /**
   * @param string
   */
  public function setArtifactsGcsBucket($artifactsGcsBucket)
  {
    $this->artifactsGcsBucket = $artifactsGcsBucket;
  }
  /**
   * @return string
   */
  public function getArtifactsGcsBucket()
  {
    return $this->artifactsGcsBucket;
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
   * @param string
   */
  public function setDeleteBuild($deleteBuild)
  {
    $this->deleteBuild = $deleteBuild;
  }
  /**
   * @return string
   */
  public function getDeleteBuild()
  {
    return $this->deleteBuild;
  }
  /**
   * @param string
   */
  public function setDeleteLogs($deleteLogs)
  {
    $this->deleteLogs = $deleteLogs;
  }
  /**
   * @return string
   */
  public function getDeleteLogs()
  {
    return $this->deleteLogs;
  }
  /**
   * @param ApplyResults
   */
  public function setDeleteResults(ApplyResults $deleteResults)
  {
    $this->deleteResults = $deleteResults;
  }
  /**
   * @return ApplyResults
   */
  public function getDeleteResults()
  {
    return $this->deleteResults;
  }
  /**
   * @param string
   */
  public function setErrorCode($errorCode)
  {
    $this->errorCode = $errorCode;
  }
  /**
   * @return string
   */
  public function getErrorCode()
  {
    return $this->errorCode;
  }
  /**
   * @param string
   */
  public function setErrorLogs($errorLogs)
  {
    $this->errorLogs = $errorLogs;
  }
  /**
   * @return string
   */
  public function getErrorLogs()
  {
    return $this->errorLogs;
  }
  /**
   * @param bool
   */
  public function setImportExistingResources($importExistingResources)
  {
    $this->importExistingResources = $importExistingResources;
  }
  /**
   * @return bool
   */
  public function getImportExistingResources()
  {
    return $this->importExistingResources;
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
  public function setLatestRevision($latestRevision)
  {
    $this->latestRevision = $latestRevision;
  }
  /**
   * @return string
   */
  public function getLatestRevision()
  {
    return $this->latestRevision;
  }
  /**
   * @param string
   */
  public function setLockState($lockState)
  {
    $this->lockState = $lockState;
  }
  /**
   * @return string
   */
  public function getLockState()
  {
    return $this->lockState;
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
  public function setQuotaValidation($quotaValidation)
  {
    $this->quotaValidation = $quotaValidation;
  }
  /**
   * @return string
   */
  public function getQuotaValidation()
  {
    return $this->quotaValidation;
  }
  /**
   * @param string
   */
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return string
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
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
  public function setStateDetail($stateDetail)
  {
    $this->stateDetail = $stateDetail;
  }
  /**
   * @return string
   */
  public function getStateDetail()
  {
    return $this->stateDetail;
  }
  /**
   * @param TerraformBlueprint
   */
  public function setTerraformBlueprint(TerraformBlueprint $terraformBlueprint)
  {
    $this->terraformBlueprint = $terraformBlueprint;
  }
  /**
   * @return TerraformBlueprint
   */
  public function getTerraformBlueprint()
  {
    return $this->terraformBlueprint;
  }
  /**
   * @param TerraformError[]
   */
  public function setTfErrors($tfErrors)
  {
    $this->tfErrors = $tfErrors;
  }
  /**
   * @return TerraformError[]
   */
  public function getTfErrors()
  {
    return $this->tfErrors;
  }
  /**
   * @param string
   */
  public function setTfVersion($tfVersion)
  {
    $this->tfVersion = $tfVersion;
  }
  /**
   * @return string
   */
  public function getTfVersion()
  {
    return $this->tfVersion;
  }
  /**
   * @param string
   */
  public function setTfVersionConstraint($tfVersionConstraint)
  {
    $this->tfVersionConstraint = $tfVersionConstraint;
  }
  /**
   * @return string
   */
  public function getTfVersionConstraint()
  {
    return $this->tfVersionConstraint;
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
  /**
   * @param string
   */
  public function setWorkerPool($workerPool)
  {
    $this->workerPool = $workerPool;
  }
  /**
   * @return string
   */
  public function getWorkerPool()
  {
    return $this->workerPool;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Deployment::class, 'Google_Service_Config_Deployment');
