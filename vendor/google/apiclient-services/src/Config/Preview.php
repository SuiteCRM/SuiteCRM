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

class Preview extends \Google\Collection
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
  public $build;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $deployment;
  /**
   * @var string
   */
  public $errorCode;
  /**
   * @var string
   */
  public $errorLogs;
  protected $errorStatusType = Status::class;
  protected $errorStatusDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $logs;
  /**
   * @var string
   */
  public $name;
  protected $previewArtifactsType = PreviewArtifacts::class;
  protected $previewArtifactsDataType = '';
  /**
   * @var string
   */
  public $previewMode;
  /**
   * @var string
   */
  public $serviceAccount;
  /**
   * @var string
   */
  public $state;
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
  public function setBuild($build)
  {
    $this->build = $build;
  }
  /**
   * @return string
   */
  public function getBuild()
  {
    return $this->build;
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
  public function setDeployment($deployment)
  {
    $this->deployment = $deployment;
  }
  /**
   * @return string
   */
  public function getDeployment()
  {
    return $this->deployment;
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
   * @param Status
   */
  public function setErrorStatus(Status $errorStatus)
  {
    $this->errorStatus = $errorStatus;
  }
  /**
   * @return Status
   */
  public function getErrorStatus()
  {
    return $this->errorStatus;
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
  public function setLogs($logs)
  {
    $this->logs = $logs;
  }
  /**
   * @return string
   */
  public function getLogs()
  {
    return $this->logs;
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
   * @param PreviewArtifacts
   */
  public function setPreviewArtifacts(PreviewArtifacts $previewArtifacts)
  {
    $this->previewArtifacts = $previewArtifacts;
  }
  /**
   * @return PreviewArtifacts
   */
  public function getPreviewArtifacts()
  {
    return $this->previewArtifacts;
  }
  /**
   * @param string
   */
  public function setPreviewMode($previewMode)
  {
    $this->previewMode = $previewMode;
  }
  /**
   * @return string
   */
  public function getPreviewMode()
  {
    return $this->previewMode;
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
class_alias(Preview::class, 'Google_Service_Config_Preview');
