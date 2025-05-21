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

namespace Google\Service\CloudNaturalLanguage;

class XPSTextComponentModel extends \Google\Model
{
  /**
   * @var string
   */
  public $batchPredictionModelGcsUri;
  /**
   * @var string
   */
  public $onlinePredictionModelGcsUri;
  /**
   * @var string
   */
  public $partition;
  protected $servingArtifactType = XPSModelArtifactItem::class;
  protected $servingArtifactDataType = '';
  /**
   * @var string
   */
  public $servoModelName;
  /**
   * @var string
   */
  public $submodelName;
  /**
   * @var string
   */
  public $submodelType;
  /**
   * @var string
   */
  public $tfRuntimeVersion;
  /**
   * @var string
   */
  public $versionNumber;

  /**
   * @param string
   */
  public function setBatchPredictionModelGcsUri($batchPredictionModelGcsUri)
  {
    $this->batchPredictionModelGcsUri = $batchPredictionModelGcsUri;
  }
  /**
   * @return string
   */
  public function getBatchPredictionModelGcsUri()
  {
    return $this->batchPredictionModelGcsUri;
  }
  /**
   * @param string
   */
  public function setOnlinePredictionModelGcsUri($onlinePredictionModelGcsUri)
  {
    $this->onlinePredictionModelGcsUri = $onlinePredictionModelGcsUri;
  }
  /**
   * @return string
   */
  public function getOnlinePredictionModelGcsUri()
  {
    return $this->onlinePredictionModelGcsUri;
  }
  /**
   * @param string
   */
  public function setPartition($partition)
  {
    $this->partition = $partition;
  }
  /**
   * @return string
   */
  public function getPartition()
  {
    return $this->partition;
  }
  /**
   * @param XPSModelArtifactItem
   */
  public function setServingArtifact(XPSModelArtifactItem $servingArtifact)
  {
    $this->servingArtifact = $servingArtifact;
  }
  /**
   * @return XPSModelArtifactItem
   */
  public function getServingArtifact()
  {
    return $this->servingArtifact;
  }
  /**
   * @param string
   */
  public function setServoModelName($servoModelName)
  {
    $this->servoModelName = $servoModelName;
  }
  /**
   * @return string
   */
  public function getServoModelName()
  {
    return $this->servoModelName;
  }
  /**
   * @param string
   */
  public function setSubmodelName($submodelName)
  {
    $this->submodelName = $submodelName;
  }
  /**
   * @return string
   */
  public function getSubmodelName()
  {
    return $this->submodelName;
  }
  /**
   * @param string
   */
  public function setSubmodelType($submodelType)
  {
    $this->submodelType = $submodelType;
  }
  /**
   * @return string
   */
  public function getSubmodelType()
  {
    return $this->submodelType;
  }
  /**
   * @param string
   */
  public function setTfRuntimeVersion($tfRuntimeVersion)
  {
    $this->tfRuntimeVersion = $tfRuntimeVersion;
  }
  /**
   * @return string
   */
  public function getTfRuntimeVersion()
  {
    return $this->tfRuntimeVersion;
  }
  /**
   * @param string
   */
  public function setVersionNumber($versionNumber)
  {
    $this->versionNumber = $versionNumber;
  }
  /**
   * @return string
   */
  public function getVersionNumber()
  {
    return $this->versionNumber;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSTextComponentModel::class, 'Google_Service_CloudNaturalLanguage_XPSTextComponentModel');
