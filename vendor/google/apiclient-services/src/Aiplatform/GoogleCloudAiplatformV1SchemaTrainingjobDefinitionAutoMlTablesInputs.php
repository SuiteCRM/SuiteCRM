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

class GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutoMlTablesInputs extends \Google\Collection
{
  protected $collection_key = 'transformations';
  /**
   * @var string[]
   */
  public $additionalExperiments;
  /**
   * @var bool
   */
  public $disableEarlyStopping;
  protected $exportEvaluatedDataItemsConfigType = GoogleCloudAiplatformV1SchemaTrainingjobDefinitionExportEvaluatedDataItemsConfig::class;
  protected $exportEvaluatedDataItemsConfigDataType = '';
  /**
   * @var string
   */
  public $optimizationObjective;
  /**
   * @var float
   */
  public $optimizationObjectivePrecisionValue;
  /**
   * @var float
   */
  public $optimizationObjectiveRecallValue;
  /**
   * @var string
   */
  public $predictionType;
  /**
   * @var string
   */
  public $targetColumn;
  /**
   * @var string
   */
  public $trainBudgetMilliNodeHours;
  protected $transformationsType = GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutoMlTablesInputsTransformation::class;
  protected $transformationsDataType = 'array';
  /**
   * @var string
   */
  public $weightColumnName;

  /**
   * @param string[]
   */
  public function setAdditionalExperiments($additionalExperiments)
  {
    $this->additionalExperiments = $additionalExperiments;
  }
  /**
   * @return string[]
   */
  public function getAdditionalExperiments()
  {
    return $this->additionalExperiments;
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
   * @param GoogleCloudAiplatformV1SchemaTrainingjobDefinitionExportEvaluatedDataItemsConfig
   */
  public function setExportEvaluatedDataItemsConfig(GoogleCloudAiplatformV1SchemaTrainingjobDefinitionExportEvaluatedDataItemsConfig $exportEvaluatedDataItemsConfig)
  {
    $this->exportEvaluatedDataItemsConfig = $exportEvaluatedDataItemsConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaTrainingjobDefinitionExportEvaluatedDataItemsConfig
   */
  public function getExportEvaluatedDataItemsConfig()
  {
    return $this->exportEvaluatedDataItemsConfig;
  }
  /**
   * @param string
   */
  public function setOptimizationObjective($optimizationObjective)
  {
    $this->optimizationObjective = $optimizationObjective;
  }
  /**
   * @return string
   */
  public function getOptimizationObjective()
  {
    return $this->optimizationObjective;
  }
  /**
   * @param float
   */
  public function setOptimizationObjectivePrecisionValue($optimizationObjectivePrecisionValue)
  {
    $this->optimizationObjectivePrecisionValue = $optimizationObjectivePrecisionValue;
  }
  /**
   * @return float
   */
  public function getOptimizationObjectivePrecisionValue()
  {
    return $this->optimizationObjectivePrecisionValue;
  }
  /**
   * @param float
   */
  public function setOptimizationObjectiveRecallValue($optimizationObjectiveRecallValue)
  {
    $this->optimizationObjectiveRecallValue = $optimizationObjectiveRecallValue;
  }
  /**
   * @return float
   */
  public function getOptimizationObjectiveRecallValue()
  {
    return $this->optimizationObjectiveRecallValue;
  }
  /**
   * @param string
   */
  public function setPredictionType($predictionType)
  {
    $this->predictionType = $predictionType;
  }
  /**
   * @return string
   */
  public function getPredictionType()
  {
    return $this->predictionType;
  }
  /**
   * @param string
   */
  public function setTargetColumn($targetColumn)
  {
    $this->targetColumn = $targetColumn;
  }
  /**
   * @return string
   */
  public function getTargetColumn()
  {
    return $this->targetColumn;
  }
  /**
   * @param string
   */
  public function setTrainBudgetMilliNodeHours($trainBudgetMilliNodeHours)
  {
    $this->trainBudgetMilliNodeHours = $trainBudgetMilliNodeHours;
  }
  /**
   * @return string
   */
  public function getTrainBudgetMilliNodeHours()
  {
    return $this->trainBudgetMilliNodeHours;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutoMlTablesInputsTransformation[]
   */
  public function setTransformations($transformations)
  {
    $this->transformations = $transformations;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutoMlTablesInputsTransformation[]
   */
  public function getTransformations()
  {
    return $this->transformations;
  }
  /**
   * @param string
   */
  public function setWeightColumnName($weightColumnName)
  {
    $this->weightColumnName = $weightColumnName;
  }
  /**
   * @return string
   */
  public function getWeightColumnName()
  {
    return $this->weightColumnName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutoMlTablesInputs::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaTrainingjobDefinitionAutoMlTablesInputs');
