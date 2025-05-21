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

namespace Google\Service\Bigquery;

class TrainingOptions extends \Google\Collection
{
  protected $collection_key = 'vertexAiModelVersionAliases';
  /**
   * @var string
   */
  public $activationFn;
  /**
   * @var bool
   */
  public $adjustStepChanges;
  /**
   * @var bool
   */
  public $approxGlobalFeatureContrib;
  /**
   * @var bool
   */
  public $autoArima;
  /**
   * @var string
   */
  public $autoArimaMaxOrder;
  /**
   * @var string
   */
  public $autoArimaMinOrder;
  /**
   * @var bool
   */
  public $autoClassWeights;
  /**
   * @var string
   */
  public $batchSize;
  /**
   * @var string
   */
  public $boosterType;
  public $budgetHours;
  /**
   * @var bool
   */
  public $calculatePValues;
  /**
   * @var string
   */
  public $categoryEncodingMethod;
  /**
   * @var bool
   */
  public $cleanSpikesAndDips;
  /**
   * @var string
   */
  public $colorSpace;
  public $colsampleBylevel;
  public $colsampleBynode;
  public $colsampleBytree;
  /**
   * @var string
   */
  public $contributionMetric;
  /**
   * @var string
   */
  public $dartNormalizeType;
  /**
   * @var string
   */
  public $dataFrequency;
  /**
   * @var string
   */
  public $dataSplitColumn;
  public $dataSplitEvalFraction;
  /**
   * @var string
   */
  public $dataSplitMethod;
  /**
   * @var bool
   */
  public $decomposeTimeSeries;
  /**
   * @var string[]
   */
  public $dimensionIdColumns;
  /**
   * @var string
   */
  public $distanceType;
  public $dropout;
  /**
   * @var bool
   */
  public $earlyStop;
  /**
   * @var bool
   */
  public $enableGlobalExplain;
  /**
   * @var string
   */
  public $feedbackType;
  /**
   * @var bool
   */
  public $fitIntercept;
  public $forecastLimitLowerBound;
  public $forecastLimitUpperBound;
  /**
   * @var string[]
   */
  public $hiddenUnits;
  /**
   * @var string
   */
  public $holidayRegion;
  /**
   * @var string[]
   */
  public $holidayRegions;
  /**
   * @var string
   */
  public $horizon;
  /**
   * @var string[]
   */
  public $hparamTuningObjectives;
  /**
   * @var bool
   */
  public $includeDrift;
  public $initialLearnRate;
  /**
   * @var string[]
   */
  public $inputLabelColumns;
  /**
   * @var string
   */
  public $instanceWeightColumn;
  /**
   * @var string
   */
  public $integratedGradientsNumSteps;
  /**
   * @var string
   */
  public $isTestColumn;
  /**
   * @var string
   */
  public $itemColumn;
  /**
   * @var string
   */
  public $kmeansInitializationColumn;
  /**
   * @var string
   */
  public $kmeansInitializationMethod;
  public $l1RegActivation;
  public $l1Regularization;
  public $l2Regularization;
  public $labelClassWeights;
  public $learnRate;
  /**
   * @var string
   */
  public $learnRateStrategy;
  /**
   * @var string
   */
  public $lossType;
  /**
   * @var string
   */
  public $maxIterations;
  /**
   * @var string
   */
  public $maxParallelTrials;
  /**
   * @var string
   */
  public $maxTimeSeriesLength;
  /**
   * @var string
   */
  public $maxTreeDepth;
  public $minAprioriSupport;
  public $minRelativeProgress;
  public $minSplitLoss;
  /**
   * @var string
   */
  public $minTimeSeriesLength;
  /**
   * @var string
   */
  public $minTreeChildWeight;
  /**
   * @var string
   */
  public $modelRegistry;
  /**
   * @var string
   */
  public $modelUri;
  protected $nonSeasonalOrderType = ArimaOrder::class;
  protected $nonSeasonalOrderDataType = '';
  /**
   * @var string
   */
  public $numClusters;
  /**
   * @var string
   */
  public $numFactors;
  /**
   * @var string
   */
  public $numParallelTree;
  /**
   * @var string
   */
  public $numPrincipalComponents;
  /**
   * @var string
   */
  public $numTrials;
  /**
   * @var string
   */
  public $optimizationStrategy;
  /**
   * @var string
   */
  public $optimizer;
  public $pcaExplainedVarianceRatio;
  /**
   * @var string
   */
  public $pcaSolver;
  /**
   * @var string
   */
  public $sampledShapleyNumPaths;
  /**
   * @var bool
   */
  public $scaleFeatures;
  /**
   * @var bool
   */
  public $standardizeFeatures;
  public $subsample;
  /**
   * @var string
   */
  public $tfVersion;
  /**
   * @var string
   */
  public $timeSeriesDataColumn;
  /**
   * @var string
   */
  public $timeSeriesIdColumn;
  /**
   * @var string[]
   */
  public $timeSeriesIdColumns;
  public $timeSeriesLengthFraction;
  /**
   * @var string
   */
  public $timeSeriesTimestampColumn;
  /**
   * @var string
   */
  public $treeMethod;
  /**
   * @var string
   */
  public $trendSmoothingWindowSize;
  /**
   * @var string
   */
  public $userColumn;
  /**
   * @var string[]
   */
  public $vertexAiModelVersionAliases;
  public $walsAlpha;
  /**
   * @var bool
   */
  public $warmStart;
  /**
   * @var string
   */
  public $xgboostVersion;

  /**
   * @param string
   */
  public function setActivationFn($activationFn)
  {
    $this->activationFn = $activationFn;
  }
  /**
   * @return string
   */
  public function getActivationFn()
  {
    return $this->activationFn;
  }
  /**
   * @param bool
   */
  public function setAdjustStepChanges($adjustStepChanges)
  {
    $this->adjustStepChanges = $adjustStepChanges;
  }
  /**
   * @return bool
   */
  public function getAdjustStepChanges()
  {
    return $this->adjustStepChanges;
  }
  /**
   * @param bool
   */
  public function setApproxGlobalFeatureContrib($approxGlobalFeatureContrib)
  {
    $this->approxGlobalFeatureContrib = $approxGlobalFeatureContrib;
  }
  /**
   * @return bool
   */
  public function getApproxGlobalFeatureContrib()
  {
    return $this->approxGlobalFeatureContrib;
  }
  /**
   * @param bool
   */
  public function setAutoArima($autoArima)
  {
    $this->autoArima = $autoArima;
  }
  /**
   * @return bool
   */
  public function getAutoArima()
  {
    return $this->autoArima;
  }
  /**
   * @param string
   */
  public function setAutoArimaMaxOrder($autoArimaMaxOrder)
  {
    $this->autoArimaMaxOrder = $autoArimaMaxOrder;
  }
  /**
   * @return string
   */
  public function getAutoArimaMaxOrder()
  {
    return $this->autoArimaMaxOrder;
  }
  /**
   * @param string
   */
  public function setAutoArimaMinOrder($autoArimaMinOrder)
  {
    $this->autoArimaMinOrder = $autoArimaMinOrder;
  }
  /**
   * @return string
   */
  public function getAutoArimaMinOrder()
  {
    return $this->autoArimaMinOrder;
  }
  /**
   * @param bool
   */
  public function setAutoClassWeights($autoClassWeights)
  {
    $this->autoClassWeights = $autoClassWeights;
  }
  /**
   * @return bool
   */
  public function getAutoClassWeights()
  {
    return $this->autoClassWeights;
  }
  /**
   * @param string
   */
  public function setBatchSize($batchSize)
  {
    $this->batchSize = $batchSize;
  }
  /**
   * @return string
   */
  public function getBatchSize()
  {
    return $this->batchSize;
  }
  /**
   * @param string
   */
  public function setBoosterType($boosterType)
  {
    $this->boosterType = $boosterType;
  }
  /**
   * @return string
   */
  public function getBoosterType()
  {
    return $this->boosterType;
  }
  public function setBudgetHours($budgetHours)
  {
    $this->budgetHours = $budgetHours;
  }
  public function getBudgetHours()
  {
    return $this->budgetHours;
  }
  /**
   * @param bool
   */
  public function setCalculatePValues($calculatePValues)
  {
    $this->calculatePValues = $calculatePValues;
  }
  /**
   * @return bool
   */
  public function getCalculatePValues()
  {
    return $this->calculatePValues;
  }
  /**
   * @param string
   */
  public function setCategoryEncodingMethod($categoryEncodingMethod)
  {
    $this->categoryEncodingMethod = $categoryEncodingMethod;
  }
  /**
   * @return string
   */
  public function getCategoryEncodingMethod()
  {
    return $this->categoryEncodingMethod;
  }
  /**
   * @param bool
   */
  public function setCleanSpikesAndDips($cleanSpikesAndDips)
  {
    $this->cleanSpikesAndDips = $cleanSpikesAndDips;
  }
  /**
   * @return bool
   */
  public function getCleanSpikesAndDips()
  {
    return $this->cleanSpikesAndDips;
  }
  /**
   * @param string
   */
  public function setColorSpace($colorSpace)
  {
    $this->colorSpace = $colorSpace;
  }
  /**
   * @return string
   */
  public function getColorSpace()
  {
    return $this->colorSpace;
  }
  public function setColsampleBylevel($colsampleBylevel)
  {
    $this->colsampleBylevel = $colsampleBylevel;
  }
  public function getColsampleBylevel()
  {
    return $this->colsampleBylevel;
  }
  public function setColsampleBynode($colsampleBynode)
  {
    $this->colsampleBynode = $colsampleBynode;
  }
  public function getColsampleBynode()
  {
    return $this->colsampleBynode;
  }
  public function setColsampleBytree($colsampleBytree)
  {
    $this->colsampleBytree = $colsampleBytree;
  }
  public function getColsampleBytree()
  {
    return $this->colsampleBytree;
  }
  /**
   * @param string
   */
  public function setContributionMetric($contributionMetric)
  {
    $this->contributionMetric = $contributionMetric;
  }
  /**
   * @return string
   */
  public function getContributionMetric()
  {
    return $this->contributionMetric;
  }
  /**
   * @param string
   */
  public function setDartNormalizeType($dartNormalizeType)
  {
    $this->dartNormalizeType = $dartNormalizeType;
  }
  /**
   * @return string
   */
  public function getDartNormalizeType()
  {
    return $this->dartNormalizeType;
  }
  /**
   * @param string
   */
  public function setDataFrequency($dataFrequency)
  {
    $this->dataFrequency = $dataFrequency;
  }
  /**
   * @return string
   */
  public function getDataFrequency()
  {
    return $this->dataFrequency;
  }
  /**
   * @param string
   */
  public function setDataSplitColumn($dataSplitColumn)
  {
    $this->dataSplitColumn = $dataSplitColumn;
  }
  /**
   * @return string
   */
  public function getDataSplitColumn()
  {
    return $this->dataSplitColumn;
  }
  public function setDataSplitEvalFraction($dataSplitEvalFraction)
  {
    $this->dataSplitEvalFraction = $dataSplitEvalFraction;
  }
  public function getDataSplitEvalFraction()
  {
    return $this->dataSplitEvalFraction;
  }
  /**
   * @param string
   */
  public function setDataSplitMethod($dataSplitMethod)
  {
    $this->dataSplitMethod = $dataSplitMethod;
  }
  /**
   * @return string
   */
  public function getDataSplitMethod()
  {
    return $this->dataSplitMethod;
  }
  /**
   * @param bool
   */
  public function setDecomposeTimeSeries($decomposeTimeSeries)
  {
    $this->decomposeTimeSeries = $decomposeTimeSeries;
  }
  /**
   * @return bool
   */
  public function getDecomposeTimeSeries()
  {
    return $this->decomposeTimeSeries;
  }
  /**
   * @param string[]
   */
  public function setDimensionIdColumns($dimensionIdColumns)
  {
    $this->dimensionIdColumns = $dimensionIdColumns;
  }
  /**
   * @return string[]
   */
  public function getDimensionIdColumns()
  {
    return $this->dimensionIdColumns;
  }
  /**
   * @param string
   */
  public function setDistanceType($distanceType)
  {
    $this->distanceType = $distanceType;
  }
  /**
   * @return string
   */
  public function getDistanceType()
  {
    return $this->distanceType;
  }
  public function setDropout($dropout)
  {
    $this->dropout = $dropout;
  }
  public function getDropout()
  {
    return $this->dropout;
  }
  /**
   * @param bool
   */
  public function setEarlyStop($earlyStop)
  {
    $this->earlyStop = $earlyStop;
  }
  /**
   * @return bool
   */
  public function getEarlyStop()
  {
    return $this->earlyStop;
  }
  /**
   * @param bool
   */
  public function setEnableGlobalExplain($enableGlobalExplain)
  {
    $this->enableGlobalExplain = $enableGlobalExplain;
  }
  /**
   * @return bool
   */
  public function getEnableGlobalExplain()
  {
    return $this->enableGlobalExplain;
  }
  /**
   * @param string
   */
  public function setFeedbackType($feedbackType)
  {
    $this->feedbackType = $feedbackType;
  }
  /**
   * @return string
   */
  public function getFeedbackType()
  {
    return $this->feedbackType;
  }
  /**
   * @param bool
   */
  public function setFitIntercept($fitIntercept)
  {
    $this->fitIntercept = $fitIntercept;
  }
  /**
   * @return bool
   */
  public function getFitIntercept()
  {
    return $this->fitIntercept;
  }
  public function setForecastLimitLowerBound($forecastLimitLowerBound)
  {
    $this->forecastLimitLowerBound = $forecastLimitLowerBound;
  }
  public function getForecastLimitLowerBound()
  {
    return $this->forecastLimitLowerBound;
  }
  public function setForecastLimitUpperBound($forecastLimitUpperBound)
  {
    $this->forecastLimitUpperBound = $forecastLimitUpperBound;
  }
  public function getForecastLimitUpperBound()
  {
    return $this->forecastLimitUpperBound;
  }
  /**
   * @param string[]
   */
  public function setHiddenUnits($hiddenUnits)
  {
    $this->hiddenUnits = $hiddenUnits;
  }
  /**
   * @return string[]
   */
  public function getHiddenUnits()
  {
    return $this->hiddenUnits;
  }
  /**
   * @param string
   */
  public function setHolidayRegion($holidayRegion)
  {
    $this->holidayRegion = $holidayRegion;
  }
  /**
   * @return string
   */
  public function getHolidayRegion()
  {
    return $this->holidayRegion;
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
  public function setHorizon($horizon)
  {
    $this->horizon = $horizon;
  }
  /**
   * @return string
   */
  public function getHorizon()
  {
    return $this->horizon;
  }
  /**
   * @param string[]
   */
  public function setHparamTuningObjectives($hparamTuningObjectives)
  {
    $this->hparamTuningObjectives = $hparamTuningObjectives;
  }
  /**
   * @return string[]
   */
  public function getHparamTuningObjectives()
  {
    return $this->hparamTuningObjectives;
  }
  /**
   * @param bool
   */
  public function setIncludeDrift($includeDrift)
  {
    $this->includeDrift = $includeDrift;
  }
  /**
   * @return bool
   */
  public function getIncludeDrift()
  {
    return $this->includeDrift;
  }
  public function setInitialLearnRate($initialLearnRate)
  {
    $this->initialLearnRate = $initialLearnRate;
  }
  public function getInitialLearnRate()
  {
    return $this->initialLearnRate;
  }
  /**
   * @param string[]
   */
  public function setInputLabelColumns($inputLabelColumns)
  {
    $this->inputLabelColumns = $inputLabelColumns;
  }
  /**
   * @return string[]
   */
  public function getInputLabelColumns()
  {
    return $this->inputLabelColumns;
  }
  /**
   * @param string
   */
  public function setInstanceWeightColumn($instanceWeightColumn)
  {
    $this->instanceWeightColumn = $instanceWeightColumn;
  }
  /**
   * @return string
   */
  public function getInstanceWeightColumn()
  {
    return $this->instanceWeightColumn;
  }
  /**
   * @param string
   */
  public function setIntegratedGradientsNumSteps($integratedGradientsNumSteps)
  {
    $this->integratedGradientsNumSteps = $integratedGradientsNumSteps;
  }
  /**
   * @return string
   */
  public function getIntegratedGradientsNumSteps()
  {
    return $this->integratedGradientsNumSteps;
  }
  /**
   * @param string
   */
  public function setIsTestColumn($isTestColumn)
  {
    $this->isTestColumn = $isTestColumn;
  }
  /**
   * @return string
   */
  public function getIsTestColumn()
  {
    return $this->isTestColumn;
  }
  /**
   * @param string
   */
  public function setItemColumn($itemColumn)
  {
    $this->itemColumn = $itemColumn;
  }
  /**
   * @return string
   */
  public function getItemColumn()
  {
    return $this->itemColumn;
  }
  /**
   * @param string
   */
  public function setKmeansInitializationColumn($kmeansInitializationColumn)
  {
    $this->kmeansInitializationColumn = $kmeansInitializationColumn;
  }
  /**
   * @return string
   */
  public function getKmeansInitializationColumn()
  {
    return $this->kmeansInitializationColumn;
  }
  /**
   * @param string
   */
  public function setKmeansInitializationMethod($kmeansInitializationMethod)
  {
    $this->kmeansInitializationMethod = $kmeansInitializationMethod;
  }
  /**
   * @return string
   */
  public function getKmeansInitializationMethod()
  {
    return $this->kmeansInitializationMethod;
  }
  public function setL1RegActivation($l1RegActivation)
  {
    $this->l1RegActivation = $l1RegActivation;
  }
  public function getL1RegActivation()
  {
    return $this->l1RegActivation;
  }
  public function setL1Regularization($l1Regularization)
  {
    $this->l1Regularization = $l1Regularization;
  }
  public function getL1Regularization()
  {
    return $this->l1Regularization;
  }
  public function setL2Regularization($l2Regularization)
  {
    $this->l2Regularization = $l2Regularization;
  }
  public function getL2Regularization()
  {
    return $this->l2Regularization;
  }
  public function setLabelClassWeights($labelClassWeights)
  {
    $this->labelClassWeights = $labelClassWeights;
  }
  public function getLabelClassWeights()
  {
    return $this->labelClassWeights;
  }
  public function setLearnRate($learnRate)
  {
    $this->learnRate = $learnRate;
  }
  public function getLearnRate()
  {
    return $this->learnRate;
  }
  /**
   * @param string
   */
  public function setLearnRateStrategy($learnRateStrategy)
  {
    $this->learnRateStrategy = $learnRateStrategy;
  }
  /**
   * @return string
   */
  public function getLearnRateStrategy()
  {
    return $this->learnRateStrategy;
  }
  /**
   * @param string
   */
  public function setLossType($lossType)
  {
    $this->lossType = $lossType;
  }
  /**
   * @return string
   */
  public function getLossType()
  {
    return $this->lossType;
  }
  /**
   * @param string
   */
  public function setMaxIterations($maxIterations)
  {
    $this->maxIterations = $maxIterations;
  }
  /**
   * @return string
   */
  public function getMaxIterations()
  {
    return $this->maxIterations;
  }
  /**
   * @param string
   */
  public function setMaxParallelTrials($maxParallelTrials)
  {
    $this->maxParallelTrials = $maxParallelTrials;
  }
  /**
   * @return string
   */
  public function getMaxParallelTrials()
  {
    return $this->maxParallelTrials;
  }
  /**
   * @param string
   */
  public function setMaxTimeSeriesLength($maxTimeSeriesLength)
  {
    $this->maxTimeSeriesLength = $maxTimeSeriesLength;
  }
  /**
   * @return string
   */
  public function getMaxTimeSeriesLength()
  {
    return $this->maxTimeSeriesLength;
  }
  /**
   * @param string
   */
  public function setMaxTreeDepth($maxTreeDepth)
  {
    $this->maxTreeDepth = $maxTreeDepth;
  }
  /**
   * @return string
   */
  public function getMaxTreeDepth()
  {
    return $this->maxTreeDepth;
  }
  public function setMinAprioriSupport($minAprioriSupport)
  {
    $this->minAprioriSupport = $minAprioriSupport;
  }
  public function getMinAprioriSupport()
  {
    return $this->minAprioriSupport;
  }
  public function setMinRelativeProgress($minRelativeProgress)
  {
    $this->minRelativeProgress = $minRelativeProgress;
  }
  public function getMinRelativeProgress()
  {
    return $this->minRelativeProgress;
  }
  public function setMinSplitLoss($minSplitLoss)
  {
    $this->minSplitLoss = $minSplitLoss;
  }
  public function getMinSplitLoss()
  {
    return $this->minSplitLoss;
  }
  /**
   * @param string
   */
  public function setMinTimeSeriesLength($minTimeSeriesLength)
  {
    $this->minTimeSeriesLength = $minTimeSeriesLength;
  }
  /**
   * @return string
   */
  public function getMinTimeSeriesLength()
  {
    return $this->minTimeSeriesLength;
  }
  /**
   * @param string
   */
  public function setMinTreeChildWeight($minTreeChildWeight)
  {
    $this->minTreeChildWeight = $minTreeChildWeight;
  }
  /**
   * @return string
   */
  public function getMinTreeChildWeight()
  {
    return $this->minTreeChildWeight;
  }
  /**
   * @param string
   */
  public function setModelRegistry($modelRegistry)
  {
    $this->modelRegistry = $modelRegistry;
  }
  /**
   * @return string
   */
  public function getModelRegistry()
  {
    return $this->modelRegistry;
  }
  /**
   * @param string
   */
  public function setModelUri($modelUri)
  {
    $this->modelUri = $modelUri;
  }
  /**
   * @return string
   */
  public function getModelUri()
  {
    return $this->modelUri;
  }
  /**
   * @param ArimaOrder
   */
  public function setNonSeasonalOrder(ArimaOrder $nonSeasonalOrder)
  {
    $this->nonSeasonalOrder = $nonSeasonalOrder;
  }
  /**
   * @return ArimaOrder
   */
  public function getNonSeasonalOrder()
  {
    return $this->nonSeasonalOrder;
  }
  /**
   * @param string
   */
  public function setNumClusters($numClusters)
  {
    $this->numClusters = $numClusters;
  }
  /**
   * @return string
   */
  public function getNumClusters()
  {
    return $this->numClusters;
  }
  /**
   * @param string
   */
  public function setNumFactors($numFactors)
  {
    $this->numFactors = $numFactors;
  }
  /**
   * @return string
   */
  public function getNumFactors()
  {
    return $this->numFactors;
  }
  /**
   * @param string
   */
  public function setNumParallelTree($numParallelTree)
  {
    $this->numParallelTree = $numParallelTree;
  }
  /**
   * @return string
   */
  public function getNumParallelTree()
  {
    return $this->numParallelTree;
  }
  /**
   * @param string
   */
  public function setNumPrincipalComponents($numPrincipalComponents)
  {
    $this->numPrincipalComponents = $numPrincipalComponents;
  }
  /**
   * @return string
   */
  public function getNumPrincipalComponents()
  {
    return $this->numPrincipalComponents;
  }
  /**
   * @param string
   */
  public function setNumTrials($numTrials)
  {
    $this->numTrials = $numTrials;
  }
  /**
   * @return string
   */
  public function getNumTrials()
  {
    return $this->numTrials;
  }
  /**
   * @param string
   */
  public function setOptimizationStrategy($optimizationStrategy)
  {
    $this->optimizationStrategy = $optimizationStrategy;
  }
  /**
   * @return string
   */
  public function getOptimizationStrategy()
  {
    return $this->optimizationStrategy;
  }
  /**
   * @param string
   */
  public function setOptimizer($optimizer)
  {
    $this->optimizer = $optimizer;
  }
  /**
   * @return string
   */
  public function getOptimizer()
  {
    return $this->optimizer;
  }
  public function setPcaExplainedVarianceRatio($pcaExplainedVarianceRatio)
  {
    $this->pcaExplainedVarianceRatio = $pcaExplainedVarianceRatio;
  }
  public function getPcaExplainedVarianceRatio()
  {
    return $this->pcaExplainedVarianceRatio;
  }
  /**
   * @param string
   */
  public function setPcaSolver($pcaSolver)
  {
    $this->pcaSolver = $pcaSolver;
  }
  /**
   * @return string
   */
  public function getPcaSolver()
  {
    return $this->pcaSolver;
  }
  /**
   * @param string
   */
  public function setSampledShapleyNumPaths($sampledShapleyNumPaths)
  {
    $this->sampledShapleyNumPaths = $sampledShapleyNumPaths;
  }
  /**
   * @return string
   */
  public function getSampledShapleyNumPaths()
  {
    return $this->sampledShapleyNumPaths;
  }
  /**
   * @param bool
   */
  public function setScaleFeatures($scaleFeatures)
  {
    $this->scaleFeatures = $scaleFeatures;
  }
  /**
   * @return bool
   */
  public function getScaleFeatures()
  {
    return $this->scaleFeatures;
  }
  /**
   * @param bool
   */
  public function setStandardizeFeatures($standardizeFeatures)
  {
    $this->standardizeFeatures = $standardizeFeatures;
  }
  /**
   * @return bool
   */
  public function getStandardizeFeatures()
  {
    return $this->standardizeFeatures;
  }
  public function setSubsample($subsample)
  {
    $this->subsample = $subsample;
  }
  public function getSubsample()
  {
    return $this->subsample;
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
  public function setTimeSeriesDataColumn($timeSeriesDataColumn)
  {
    $this->timeSeriesDataColumn = $timeSeriesDataColumn;
  }
  /**
   * @return string
   */
  public function getTimeSeriesDataColumn()
  {
    return $this->timeSeriesDataColumn;
  }
  /**
   * @param string
   */
  public function setTimeSeriesIdColumn($timeSeriesIdColumn)
  {
    $this->timeSeriesIdColumn = $timeSeriesIdColumn;
  }
  /**
   * @return string
   */
  public function getTimeSeriesIdColumn()
  {
    return $this->timeSeriesIdColumn;
  }
  /**
   * @param string[]
   */
  public function setTimeSeriesIdColumns($timeSeriesIdColumns)
  {
    $this->timeSeriesIdColumns = $timeSeriesIdColumns;
  }
  /**
   * @return string[]
   */
  public function getTimeSeriesIdColumns()
  {
    return $this->timeSeriesIdColumns;
  }
  public function setTimeSeriesLengthFraction($timeSeriesLengthFraction)
  {
    $this->timeSeriesLengthFraction = $timeSeriesLengthFraction;
  }
  public function getTimeSeriesLengthFraction()
  {
    return $this->timeSeriesLengthFraction;
  }
  /**
   * @param string
   */
  public function setTimeSeriesTimestampColumn($timeSeriesTimestampColumn)
  {
    $this->timeSeriesTimestampColumn = $timeSeriesTimestampColumn;
  }
  /**
   * @return string
   */
  public function getTimeSeriesTimestampColumn()
  {
    return $this->timeSeriesTimestampColumn;
  }
  /**
   * @param string
   */
  public function setTreeMethod($treeMethod)
  {
    $this->treeMethod = $treeMethod;
  }
  /**
   * @return string
   */
  public function getTreeMethod()
  {
    return $this->treeMethod;
  }
  /**
   * @param string
   */
  public function setTrendSmoothingWindowSize($trendSmoothingWindowSize)
  {
    $this->trendSmoothingWindowSize = $trendSmoothingWindowSize;
  }
  /**
   * @return string
   */
  public function getTrendSmoothingWindowSize()
  {
    return $this->trendSmoothingWindowSize;
  }
  /**
   * @param string
   */
  public function setUserColumn($userColumn)
  {
    $this->userColumn = $userColumn;
  }
  /**
   * @return string
   */
  public function getUserColumn()
  {
    return $this->userColumn;
  }
  /**
   * @param string[]
   */
  public function setVertexAiModelVersionAliases($vertexAiModelVersionAliases)
  {
    $this->vertexAiModelVersionAliases = $vertexAiModelVersionAliases;
  }
  /**
   * @return string[]
   */
  public function getVertexAiModelVersionAliases()
  {
    return $this->vertexAiModelVersionAliases;
  }
  public function setWalsAlpha($walsAlpha)
  {
    $this->walsAlpha = $walsAlpha;
  }
  public function getWalsAlpha()
  {
    return $this->walsAlpha;
  }
  /**
   * @param bool
   */
  public function setWarmStart($warmStart)
  {
    $this->warmStart = $warmStart;
  }
  /**
   * @return bool
   */
  public function getWarmStart()
  {
    return $this->warmStart;
  }
  /**
   * @param string
   */
  public function setXgboostVersion($xgboostVersion)
  {
    $this->xgboostVersion = $xgboostVersion;
  }
  /**
   * @return string
   */
  public function getXgboostVersion()
  {
    return $this->xgboostVersion;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TrainingOptions::class, 'Google_Service_Bigquery_TrainingOptions');
