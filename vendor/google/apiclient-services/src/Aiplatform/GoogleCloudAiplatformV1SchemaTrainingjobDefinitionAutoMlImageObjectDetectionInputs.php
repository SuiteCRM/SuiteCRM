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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutoMlImageObjectDetectionInputs extends \Google\Model
{
  /**
   * @var string
   */
  public $budgetMilliNodeHours;
  /**
   * @var bool
   */
  public $disableEarlyStopping;
  /**
   * @var string
   */
  public $modelType;
  protected $tunableParameterType = GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutomlImageTrainingTunableParameter::class;
  protected $tunableParameterDataType = '';
  /**
   * @var string
   */
  public $uptrainBaseModelId;

  /**
   * @param string
   */
  public function setBudgetMilliNodeHours($budgetMilliNodeHours)
  {
    $this->budgetMilliNodeHours = $budgetMilliNodeHours;
  }
  /**
   * @return string
   */
  public function getBudgetMilliNodeHours()
  {
    return $this->budgetMilliNodeHours;
  }
  /**
   * @param bool
   */
  public function setDisableEarlyStopping($disableEarlyStopping)
  {
    $this->disableEarlyStopping = $disableEarlyStopping;
  }
  /**
   * @return bool
   */
  public function getDisableEarlyStopping()
  {
    return $this->disableEarlyStopping;
  }
  /**
   * @param string
   */
  public function setModelType($modelType)
  {
    $this->modelType = $modelType;
  }
  /**
   * @return string
   */
  public function getModelType()
  {
    return $this->modelType;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutomlImageTrainingTunableParameter
   */
  public function setTunableParameter(GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutomlImageTrainingTunableParameter $tunableParameter)
  {
    $this->tunableParameter = $tunableParameter;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutomlImageTrainingTunableParameter
   */
  public function getTunableParameter()
  {
    return $this->tunableParameter;
  }
  /**
   * @param string
   */
  public function setUptrainBaseModelId($uptrainBaseModelId)
  {
    $this->uptrainBaseModelId = $uptrainBaseModelId;
  }
  /**
   * @return string
   */
  public function getUptrainBaseModelId()
  {
    return $this->uptrainBaseModelId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutoMlImageObjectDetectionInputs::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutoMlImageObjectDetectionInputs');
