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

class GoogleCloudAiplatformV1SchemaTrainingjobDefinitionTftForecastingInputs extends \Google\Collection
{
  protected $collection_key = 'unavailableAtForecastColumns';
  /**
   * @var string[]
   */
  public $additionalExperiments;
  /**
   * @var string[]
   */
  public $availableAtForecastColumns;
  /**
   * @var string
   */
  public $contextWindow;
  protected $dataGranularityType = GoogleCloudAiplatformV1SchemaTrainingjobDefinitionTftForecastingInputsGranularity::class;
  protected $dataGranularityDataType = '';
  protected $exportEvaluatedDataItemsConfigType = GoogleCloudAiplatformV1SchemaTrainingjobDefinitionExportEvaluatedDataItemsConfig::class;
  protected $exportEvaluatedDataItemsConfigDataType = '';
  /**
   * @var string
   */
  public $forecastHorizon;
  protected $hierarchyConfigType = GoogleCloudAiplatformV1SchemaTrainingjobDefinitionHierarchyConfig::class;
  protected $hierarchyConfigDataType = '';
  /**
   * @var string[]
   */
  public $holidayRegions;
  /**
   * @var string
   */
  public $optimizationObjective;
  public $quantiles;
  /**
   * @var string
   */
  public $targetColumn;
  /**
   * @var string
   */
  public $timeColumn;
  /**
   * @var string[]
   */
  public $timeSeriesAttributeColumns;
  /**
   * @var string
   */
  public $timeSeriesIdentifierColumn;
  /**
   * @var string
   */
  public $trainBudgetMilliNodeHours;
  protected $transformationsType = GoogleCloudAiplatformV1SchemaTrainingjobDefinitionTftForecastingInputsTransformation::class;
  protected $transformationsDataType = 'array';
  /**
   * @var string[]
   */
  public $unavailableAtForecastColumns;
  /**
   * @var string
   */
  public $validationOptions;
  /**
   * @var string
   */
  public $weightColumn;
  protected $windowConfigType = GoogleCloudAiplatformV1SchemaTrainingjobDefinitionWindowConfig::class;
  protected $windowConfigDataType = '';

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
   * @param string[]
   */
  public function setAvailableAtForecastColumns($availableAtForecastColumns)
  {
    $this->availableAtForecastColumns = $availableAtForecastColumns;
  }
  /**
   * @return string[]
   */
  public function getAvailableAtForecastColumns()
  {
    return $this->availableAtForecastColumns;
  }
  /**
   * @param string
   */
  public function setContextWindow($contextWindow)
  {
    $this->contextWindow = $contextWindow;
  }
  /**
   * @return string
   */
  public function getContextWindow()
  {
    return $this->contextWindow;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaTrainingjobDefinitionTftForecastingInputsGranularity
   */
  public function setDataGranularity(GoogleCloudAiplatformV1SchemaTrainingjobDefinitionTftForecastingInputsGranularity $dataGranularity)
  {
    $this->dataGranularity = $dataGranularity;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaTrainingjobDefinitionTftForecastingInputsGranularity
   */
  public function getDataGranularity()
  {
    return $this->dataGranularity;
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
  public function setForecastHorizon($forecastHorizon)
  {
    $this->forecastHorizon = $forecastHorizon;
  }
  /**
   * @return string
   */
  public function getForecastHorizon()
  {
    return $this->forecastHorizon;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaTrainingjobDefinitionHierarchyConfig
   */
  public function setHierarchyConfig(GoogleCloudAiplatformV1SchemaTrainingjobDefinitionHierarchyConfig $hierarchyConfig)
  {
    $this->hierarchyConfig = $hierarchyConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaTrainingjobDefinitionHierarchyConfig
   */
  public function getHierarchyConfig()
  {
    return $this->hierarchyConfig;
  }
  /**
   * @param string[]
   */
  public function setHolidayRegions($holidayRegions)
  {
    $this->holidayRegions = $holidayRegions;
  }
  /**
   * @return string[]
   */
  public function getHolidayRegions()
  {
    return $this->holidayRegions;
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
  public function setQuantiles($quantiles)
  {
    $this->quantiles = $quantiles;
  }
  public function getQuantiles()
  {
    return $this->quantiles;
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
  public function setTimeColumn($timeColumn)
  {
    $this->timeColumn = $timeColumn;
  }
  /**
   * @return string
   */
  public function getTimeColumn()
  {
    return $this->timeColumn;
  }
  /**
   * @param string[]
   */
  public function setTimeSeriesAttributeColumns($timeSeriesAttributeColumns)
  {
    $this->timeSeriesAttributeColumns = $timeSeriesAttributeColumns;
  }
  /**
   * @return string[]
   */
  public function getTimeSeriesAttributeColumns()
  {
    return $this->timeSeriesAttributeColumns;
  }
  /**
   * @param string
   */
  public function setTimeSeriesIdentifierColumn($timeSeriesIdentifierColumn)
  {
    $this->timeSeriesIdentifierColumn = $timeSeriesIdentifierColumn;
  }
  /**
   * @return string
   */
  public function getTimeSeriesIdentifierColumn()
  {
    return $this->timeSeriesIdentifierColumn;
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
   * @param GoogleCloudAiplatformV1SchemaTrainingjobDefinitionTftForecastingInputsTransformation[]
   */
  public function setTransformations($transformations)
  {
    $this->transformations = $transformations;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaTrainingjobDefinitionTftForecastingInputsTransformation[]
   */
  public function getTransformations()
  {
    return $this->transformations;
  }
  /**
   * @param string[]
   */
  public function setUnavailableAtForecastColumns($unavailableAtForecastColumns)
  {
    $this->unavailableAtForecastColumns = $unavailableAtForecastColumns;
  }
  /**
   * @return string[]
   */
  public function getUnavailableAtForecastColumns()
  {
    return $this->unavailableAtForecastColumns;
  }
  /**
   * @param string
   */
  public function setValidationOptions($validationOptions)
  {
    $this->validationOptions = $validationOptions;
  }
  /**
   * @return string
   */
  public function getValidationOptions()
  {
    return $this->validationOptions;
  }
  /**
   * @param string
   */
  public function setWeightColumn($weightColumn)
  {
    $this->weightColumn = $weightColumn;
  }
  /**
   * @return string
   */
  public function getWeightColumn()
  {
    return $this->weightColumn;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaTrainingjobDefinitionWindowConfig
   */
  public function setWindowConfig(GoogleCloudAiplatformV1SchemaTrainingjobDefinitionWindowConfig $windowConfig)
  {
    $this->windowConfig = $windowConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaTrainingjobDefinitionWindowConfig
   */
  public function getWindowConfig()
  {
    return $this->windowConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaTrainingjobDefinitionTftForecastingInputs::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaTrainingjobDefinitionTftForecastingInputs');
