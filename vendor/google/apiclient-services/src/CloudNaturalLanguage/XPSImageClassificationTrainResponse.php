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

class XPSImageClassificationTrainResponse extends \Google\Model
{
  /**
   * @var string
   */
  public $classCount;
  protected $exportModelSpecType = XPSImageExportModelSpec::class;
  protected $exportModelSpecDataType = '';
  protected $modelArtifactSpecType = XPSImageModelArtifactSpec::class;
  protected $modelArtifactSpecDataType = '';
  protected $modelServingSpecType = XPSImageModelServingSpec::class;
  protected $modelServingSpecDataType = '';
  /**
   * @var string
   */
  public $stopReason;
  /**
   * @var string
   */
  public $trainCostInNodeTime;
  /**
   * @var string
   */
  public $trainCostNodeSeconds;

  /**
   * @param string
   */
  public function setClassCount($classCount)
  {
    $this->classCount = $classCount;
  }
  /**
   * @return string
   */
  public function getClassCount()
  {
    return $this->classCount;
  }
  /**
   * @param XPSImageExportModelSpec
   */
  public function setExportModelSpec(XPSImageExportModelSpec $exportModelSpec)
  {
    $this->exportModelSpec = $exportModelSpec;
  }
  /**
   * @return XPSImageExportModelSpec
   */
  public function getExportModelSpec()
  {
    return $this->exportModelSpec;
  }
  /**
   * @param XPSImageModelArtifactSpec
   */
  public function setModelArtifactSpec(XPSImageModelArtifactSpec $modelArtifactSpec)
  {
    $this->modelArtifactSpec = $modelArtifactSpec;
  }
  /**
   * @return XPSImageModelArtifactSpec
   */
  public function getModelArtifactSpec()
  {
    return $this->modelArtifactSpec;
  }
  /**
   * @param XPSImageModelServingSpec
   */
  public function setModelServingSpec(XPSImageModelServingSpec $modelServingSpec)
  {
    $this->modelServingSpec = $modelServingSpec;
  }
  /**
   * @return XPSImageModelServingSpec
   */
  public function getModelServingSpec()
  {
    return $this->modelServingSpec;
  }
  /**
   * @param string
   */
  public function setStopReason($stopReason)
  {
    $this->stopReason = $stopReason;
  }
  /**
   * @return string
   */
  public function getStopReason()
  {
    return $this->stopReason;
  }
  /**
   * @param string
   */
  public function setTrainCostInNodeTime($trainCostInNodeTime)
  {
    $this->trainCostInNodeTime = $trainCostInNodeTime;
  }
  /**
   * @return string
   */
  public function getTrainCostInNodeTime()
  {
    return $this->trainCostInNodeTime;
  }
  /**
   * @param string
   */
  public function setTrainCostNodeSeconds($trainCostNodeSeconds)
  {
    $this->trainCostNodeSeconds = $trainCostNodeSeconds;
  }
  /**
   * @return string
   */
  public function getTrainCostNodeSeconds()
  {
    return $this->trainCostNodeSeconds;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSImageClassificationTrainResponse::class, 'Google_Service_CloudNaturalLanguage_XPSImageClassificationTrainResponse');
