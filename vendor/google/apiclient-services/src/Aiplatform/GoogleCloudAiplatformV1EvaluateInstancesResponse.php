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

class GoogleCloudAiplatformV1EvaluateInstancesResponse extends \Google\Model
{
  protected $bleuResultsType = GoogleCloudAiplatformV1BleuResults::class;
  protected $bleuResultsDataType = '';
  protected $coherenceResultType = GoogleCloudAiplatformV1CoherenceResult::class;
  protected $coherenceResultDataType = '';
  protected $cometResultType = GoogleCloudAiplatformV1CometResult::class;
  protected $cometResultDataType = '';
  protected $exactMatchResultsType = GoogleCloudAiplatformV1ExactMatchResults::class;
  protected $exactMatchResultsDataType = '';
  protected $fluencyResultType = GoogleCloudAiplatformV1FluencyResult::class;
  protected $fluencyResultDataType = '';
  protected $fulfillmentResultType = GoogleCloudAiplatformV1FulfillmentResult::class;
  protected $fulfillmentResultDataType = '';
  protected $groundednessResultType = GoogleCloudAiplatformV1GroundednessResult::class;
  protected $groundednessResultDataType = '';
  protected $metricxResultType = GoogleCloudAiplatformV1MetricxResult::class;
  protected $metricxResultDataType = '';
  protected $pairwiseMetricResultType = GoogleCloudAiplatformV1PairwiseMetricResult::class;
  protected $pairwiseMetricResultDataType = '';
  protected $pairwiseQuestionAnsweringQualityResultType = GoogleCloudAiplatformV1PairwiseQuestionAnsweringQualityResult::class;
  protected $pairwiseQuestionAnsweringQualityResultDataType = '';
  protected $pairwiseSummarizationQualityResultType = GoogleCloudAiplatformV1PairwiseSummarizationQualityResult::class;
  protected $pairwiseSummarizationQualityResultDataType = '';
  protected $pointwiseMetricResultType = GoogleCloudAiplatformV1PointwiseMetricResult::class;
  protected $pointwiseMetricResultDataType = '';
  protected $questionAnsweringCorrectnessResultType = GoogleCloudAiplatformV1QuestionAnsweringCorrectnessResult::class;
  protected $questionAnsweringCorrectnessResultDataType = '';
  protected $questionAnsweringHelpfulnessResultType = GoogleCloudAiplatformV1QuestionAnsweringHelpfulnessResult::class;
  protected $questionAnsweringHelpfulnessResultDataType = '';
  protected $questionAnsweringQualityResultType = GoogleCloudAiplatformV1QuestionAnsweringQualityResult::class;
  protected $questionAnsweringQualityResultDataType = '';
  protected $questionAnsweringRelevanceResultType = GoogleCloudAiplatformV1QuestionAnsweringRelevanceResult::class;
  protected $questionAnsweringRelevanceResultDataType = '';
  protected $rougeResultsType = GoogleCloudAiplatformV1RougeResults::class;
  protected $rougeResultsDataType = '';
  protected $safetyResultType = GoogleCloudAiplatformV1SafetyResult::class;
  protected $safetyResultDataType = '';
  protected $summarizationHelpfulnessResultType = GoogleCloudAiplatformV1SummarizationHelpfulnessResult::class;
  protected $summarizationHelpfulnessResultDataType = '';
  protected $summarizationQualityResultType = GoogleCloudAiplatformV1SummarizationQualityResult::class;
  protected $summarizationQualityResultDataType = '';
  protected $summarizationVerbosityResultType = GoogleCloudAiplatformV1SummarizationVerbosityResult::class;
  protected $summarizationVerbosityResultDataType = '';
  protected $toolCallValidResultsType = GoogleCloudAiplatformV1ToolCallValidResults::class;
  protected $toolCallValidResultsDataType = '';
  protected $toolNameMatchResultsType = GoogleCloudAiplatformV1ToolNameMatchResults::class;
  protected $toolNameMatchResultsDataType = '';
  protected $toolParameterKeyMatchResultsType = GoogleCloudAiplatformV1ToolParameterKeyMatchResults::class;
  protected $toolParameterKeyMatchResultsDataType = '';
  protected $toolParameterKvMatchResultsType = GoogleCloudAiplatformV1ToolParameterKVMatchResults::class;
  protected $toolParameterKvMatchResultsDataType = '';
  protected $trajectoryAnyOrderMatchResultsType = GoogleCloudAiplatformV1TrajectoryAnyOrderMatchResults::class;
  protected $trajectoryAnyOrderMatchResultsDataType = '';
  protected $trajectoryExactMatchResultsType = GoogleCloudAiplatformV1TrajectoryExactMatchResults::class;
  protected $trajectoryExactMatchResultsDataType = '';
  protected $trajectoryInOrderMatchResultsType = GoogleCloudAiplatformV1TrajectoryInOrderMatchResults::class;
  protected $trajectoryInOrderMatchResultsDataType = '';
  protected $trajectoryPrecisionResultsType = GoogleCloudAiplatformV1TrajectoryPrecisionResults::class;
  protected $trajectoryPrecisionResultsDataType = '';
  protected $trajectoryRecallResultsType = GoogleCloudAiplatformV1TrajectoryRecallResults::class;
  protected $trajectoryRecallResultsDataType = '';
  protected $trajectorySingleToolUseResultsType = GoogleCloudAiplatformV1TrajectorySingleToolUseResults::class;
  protected $trajectorySingleToolUseResultsDataType = '';

  /**
   * @param GoogleCloudAiplatformV1BleuResults
   */
  public function setBleuResults(GoogleCloudAiplatformV1BleuResults $bleuResults)
  {
    $this->bleuResults = $bleuResults;
  }
  /**
   * @return GoogleCloudAiplatformV1BleuResults
   */
  public function getBleuResults()
  {
    return $this->bleuResults;
  }
  /**
   * @param GoogleCloudAiplatformV1CoherenceResult
   */
  public function setCoherenceResult(GoogleCloudAiplatformV1CoherenceResult $coherenceResult)
  {
    $this->coherenceResult = $coherenceResult;
  }
  /**
   * @return GoogleCloudAiplatformV1CoherenceResult
   */
  public function getCoherenceResult()
  {
    return $this->coherenceResult;
  }
  /**
   * @param GoogleCloudAiplatformV1CometResult
   */
  public function setCometResult(GoogleCloudAiplatformV1CometResult $cometResult)
  {
    $this->cometResult = $cometResult;
  }
  /**
   * @return GoogleCloudAiplatformV1CometResult
   */
  public function getCometResult()
  {
    return $this->cometResult;
  }
  /**
   * @param GoogleCloudAiplatformV1ExactMatchResults
   */
  public function setExactMatchResults(GoogleCloudAiplatformV1ExactMatchResults $exactMatchResults)
  {
    $this->exactMatchResults = $exactMatchResults;
  }
  /**
   * @return GoogleCloudAiplatformV1ExactMatchResults
   */
  public function getExactMatchResults()
  {
    return $this->exactMatchResults;
  }
  /**
   * @param GoogleCloudAiplatformV1FluencyResult
   */
  public function setFluencyResult(GoogleCloudAiplatformV1FluencyResult $fluencyResult)
  {
    $this->fluencyResult = $fluencyResult;
  }
  /**
   * @return GoogleCloudAiplatformV1FluencyResult
   */
  public function getFluencyResult()
  {
    return $this->fluencyResult;
  }
  /**
   * @param GoogleCloudAiplatformV1FulfillmentResult
   */
  public function setFulfillmentResult(GoogleCloudAiplatformV1FulfillmentResult $fulfillmentResult)
  {
    $this->fulfillmentResult = $fulfillmentResult;
  }
  /**
   * @return GoogleCloudAiplatformV1FulfillmentResult
   */
  public function getFulfillmentResult()
  {
    return $this->fulfillmentResult;
  }
  /**
   * @param GoogleCloudAiplatformV1GroundednessResult
   */
  public function setGroundednessResult(GoogleCloudAiplatformV1GroundednessResult $groundednessResult)
  {
    $this->groundednessResult = $groundednessResult;
  }
  /**
   * @return GoogleCloudAiplatformV1GroundednessResult
   */
  public function getGroundednessResult()
  {
    return $this->groundednessResult;
  }
  /**
   * @param GoogleCloudAiplatformV1MetricxResult
   */
  public function setMetricxResult(GoogleCloudAiplatformV1MetricxResult $metricxResult)
  {
    $this->metricxResult = $metricxResult;
  }
  /**
   * @return GoogleCloudAiplatformV1MetricxResult
   */
  public function getMetricxResult()
  {
    return $this->metricxResult;
  }
  /**
   * @param GoogleCloudAiplatformV1PairwiseMetricResult
   */
  public function setPairwiseMetricResult(GoogleCloudAiplatformV1PairwiseMetricResult $pairwiseMetricResult)
  {
    $this->pairwiseMetricResult = $pairwiseMetricResult;
  }
  /**
   * @return GoogleCloudAiplatformV1PairwiseMetricResult
   */
  public function getPairwiseMetricResult()
  {
    return $this->pairwiseMetricResult;
  }
  /**
   * @param GoogleCloudAiplatformV1PairwiseQuestionAnsweringQualityResult
   */
  public function setPairwiseQuestionAnsweringQualityResult(GoogleCloudAiplatformV1PairwiseQuestionAnsweringQualityResult $pairwiseQuestionAnsweringQualityResult)
  {
    $this->pairwiseQuestionAnsweringQualityResult = $pairwiseQuestionAnsweringQualityResult;
  }
  /**
   * @return GoogleCloudAiplatformV1PairwiseQuestionAnsweringQualityResult
   */
  public function getPairwiseQuestionAnsweringQualityResult()
  {
    return $this->pairwiseQuestionAnsweringQualityResult;
  }
  /**
   * @param GoogleCloudAiplatformV1PairwiseSummarizationQualityResult
   */
  public function setPairwiseSummarizationQualityResult(GoogleCloudAiplatformV1PairwiseSummarizationQualityResult $pairwiseSummarizationQualityResult)
  {
    $this->pairwiseSummarizationQualityResult = $pairwiseSummarizationQualityResult;
  }
  /**
   * @return GoogleCloudAiplatformV1PairwiseSummarizationQualityResult
   */
  public function getPairwiseSummarizationQualityResult()
  {
    return $this->pairwiseSummarizationQualityResult;
  }
  /**
   * @param GoogleCloudAiplatformV1PointwiseMetricResult
   */
  public function setPointwiseMetricResult(GoogleCloudAiplatformV1PointwiseMetricResult $pointwiseMetricResult)
  {
    $this->pointwiseMetricResult = $pointwiseMetricResult;
  }
  /**
   * @return GoogleCloudAiplatformV1PointwiseMetricResult
   */
  public function getPointwiseMetricResult()
  {
    return $this->pointwiseMetricResult;
  }
  /**
   * @param GoogleCloudAiplatformV1QuestionAnsweringCorrectnessResult
   */
  public function setQuestionAnsweringCorrectnessResult(GoogleCloudAiplatformV1QuestionAnsweringCorrectnessResult $questionAnsweringCorrectnessResult)
  {
    $this->questionAnsweringCorrectnessResult = $questionAnsweringCorrectnessResult;
  }
  /**
   * @return GoogleCloudAiplatformV1QuestionAnsweringCorrectnessResult
   */
  public function getQuestionAnsweringCorrectnessResult()
  {
    return $this->questionAnsweringCorrectnessResult;
  }
  /**
   * @param GoogleCloudAiplatformV1QuestionAnsweringHelpfulnessResult
   */
  public function setQuestionAnsweringHelpfulnessResult(GoogleCloudAiplatformV1QuestionAnsweringHelpfulnessResult $questionAnsweringHelpfulnessResult)
  {
    $this->questionAnsweringHelpfulnessResult = $questionAnsweringHelpfulnessResult;
  }
  /**
   * @return GoogleCloudAiplatformV1QuestionAnsweringHelpfulnessResult
   */
  public function getQuestionAnsweringHelpfulnessResult()
  {
    return $this->questionAnsweringHelpfulnessResult;
  }
  /**
   * @param GoogleCloudAiplatformV1QuestionAnsweringQualityResult
   */
  public function setQuestionAnsweringQualityResult(GoogleCloudAiplatformV1QuestionAnsweringQualityResult $questionAnsweringQualityResult)
  {
    $this->questionAnsweringQualityResult = $questionAnsweringQualityResult;
  }
  /**
   * @return GoogleCloudAiplatformV1QuestionAnsweringQualityResult
   */
  public function getQuestionAnsweringQualityResult()
  {
    return $this->questionAnsweringQualityResult;
  }
  /**
   * @param GoogleCloudAiplatformV1QuestionAnsweringRelevanceResult
   */
  public function setQuestionAnsweringRelevanceResult(GoogleCloudAiplatformV1QuestionAnsweringRelevanceResult $questionAnsweringRelevanceResult)
  {
    $this->questionAnsweringRelevanceResult = $questionAnsweringRelevanceResult;
  }
  /**
   * @return GoogleCloudAiplatformV1QuestionAnsweringRelevanceResult
   */
  public function getQuestionAnsweringRelevanceResult()
  {
    return $this->questionAnsweringRelevanceResult;
  }
  /**
   * @param GoogleCloudAiplatformV1RougeResults
   */
  public function setRougeResults(GoogleCloudAiplatformV1RougeResults $rougeResults)
  {
    $this->rougeResults = $rougeResults;
  }
  /**
   * @return GoogleCloudAiplatformV1RougeResults
   */
  public function getRougeResults()
  {
    return $this->rougeResults;
  }
  /**
   * @param GoogleCloudAiplatformV1SafetyResult
   */
  public function setSafetyResult(GoogleCloudAiplatformV1SafetyResult $safetyResult)
  {
    $this->safetyResult = $safetyResult;
  }
  /**
   * @return GoogleCloudAiplatformV1SafetyResult
   */
  public function getSafetyResult()
  {
    return $this->safetyResult;
  }
  /**
   * @param GoogleCloudAiplatformV1SummarizationHelpfulnessResult
   */
  public function setSummarizationHelpfulnessResult(GoogleCloudAiplatformV1SummarizationHelpfulnessResult $summarizationHelpfulnessResult)
  {
    $this->summarizationHelpfulnessResult = $summarizationHelpfulnessResult;
  }
  /**
   * @return GoogleCloudAiplatformV1SummarizationHelpfulnessResult
   */
  public function getSummarizationHelpfulnessResult()
  {
    return $this->summarizationHelpfulnessResult;
  }
  /**
   * @param GoogleCloudAiplatformV1SummarizationQualityResult
   */
  public function setSummarizationQualityResult(GoogleCloudAiplatformV1SummarizationQualityResult $summarizationQualityResult)
  {
    $this->summarizationQualityResult = $summarizationQualityResult;
  }
  /**
   * @return GoogleCloudAiplatformV1SummarizationQualityResult
   */
  public function getSummarizationQualityResult()
  {
    return $this->summarizationQualityResult;
  }
  /**
   * @param GoogleCloudAiplatformV1SummarizationVerbosityResult
   */
  public function setSummarizationVerbosityResult(GoogleCloudAiplatformV1SummarizationVerbosityResult $summarizationVerbosityResult)
  {
    $this->summarizationVerbosityResult = $summarizationVerbosityResult;
  }
  /**
   * @return GoogleCloudAiplatformV1SummarizationVerbosityResult
   */
  public function getSummarizationVerbosityResult()
  {
    return $this->summarizationVerbosityResult;
  }
  /**
   * @param GoogleCloudAiplatformV1ToolCallValidResults
   */
  public function setToolCallValidResults(GoogleCloudAiplatformV1ToolCallValidResults $toolCallValidResults)
  {
    $this->toolCallValidResults = $toolCallValidResults;
  }
  /**
   * @return GoogleCloudAiplatformV1ToolCallValidResults
   */
  public function getToolCallValidResults()
  {
    return $this->toolCallValidResults;
  }
  /**
   * @param GoogleCloudAiplatformV1ToolNameMatchResults
   */
  public function setToolNameMatchResults(GoogleCloudAiplatformV1ToolNameMatchResults $toolNameMatchResults)
  {
    $this->toolNameMatchResults = $toolNameMatchResults;
  }
  /**
   * @return GoogleCloudAiplatformV1ToolNameMatchResults
   */
  public function getToolNameMatchResults()
  {
    return $this->toolNameMatchResults;
  }
  /**
   * @param GoogleCloudAiplatformV1ToolParameterKeyMatchResults
   */
  public function setToolParameterKeyMatchResults(GoogleCloudAiplatformV1ToolParameterKeyMatchResults $toolParameterKeyMatchResults)
  {
    $this->toolParameterKeyMatchResults = $toolParameterKeyMatchResults;
  }
  /**
   * @return GoogleCloudAiplatformV1ToolParameterKeyMatchResults
   */
  public function getToolParameterKeyMatchResults()
  {
    return $this->toolParameterKeyMatchResults;
  }
  /**
   * @param GoogleCloudAiplatformV1ToolParameterKVMatchResults
   */
  public function setToolParameterKvMatchResults(GoogleCloudAiplatformV1ToolParameterKVMatchResults $toolParameterKvMatchResults)
  {
    $this->toolParameterKvMatchResults = $toolParameterKvMatchResults;
  }
  /**
   * @return GoogleCloudAiplatformV1ToolParameterKVMatchResults
   */
  public function getToolParameterKvMatchResults()
  {
    return $this->toolParameterKvMatchResults;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectoryAnyOrderMatchResults
   */
  public function setTrajectoryAnyOrderMatchResults(GoogleCloudAiplatformV1TrajectoryAnyOrderMatchResults $trajectoryAnyOrderMatchResults)
  {
    $this->trajectoryAnyOrderMatchResults = $trajectoryAnyOrderMatchResults;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectoryAnyOrderMatchResults
   */
  public function getTrajectoryAnyOrderMatchResults()
  {
    return $this->trajectoryAnyOrderMatchResults;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectoryExactMatchResults
   */
  public function setTrajectoryExactMatchResults(GoogleCloudAiplatformV1TrajectoryExactMatchResults $trajectoryExactMatchResults)
  {
    $this->trajectoryExactMatchResults = $trajectoryExactMatchResults;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectoryExactMatchResults
   */
  public function getTrajectoryExactMatchResults()
  {
    return $this->trajectoryExactMatchResults;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectoryInOrderMatchResults
   */
  public function setTrajectoryInOrderMatchResults(GoogleCloudAiplatformV1TrajectoryInOrderMatchResults $trajectoryInOrderMatchResults)
  {
    $this->trajectoryInOrderMatchResults = $trajectoryInOrderMatchResults;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectoryInOrderMatchResults
   */
  public function getTrajectoryInOrderMatchResults()
  {
    return $this->trajectoryInOrderMatchResults;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectoryPrecisionResults
   */
  public function setTrajectoryPrecisionResults(GoogleCloudAiplatformV1TrajectoryPrecisionResults $trajectoryPrecisionResults)
  {
    $this->trajectoryPrecisionResults = $trajectoryPrecisionResults;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectoryPrecisionResults
   */
  public function getTrajectoryPrecisionResults()
  {
    return $this->trajectoryPrecisionResults;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectoryRecallResults
   */
  public function setTrajectoryRecallResults(GoogleCloudAiplatformV1TrajectoryRecallResults $trajectoryRecallResults)
  {
    $this->trajectoryRecallResults = $trajectoryRecallResults;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectoryRecallResults
   */
  public function getTrajectoryRecallResults()
  {
    return $this->trajectoryRecallResults;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectorySingleToolUseResults
   */
  public function setTrajectorySingleToolUseResults(GoogleCloudAiplatformV1TrajectorySingleToolUseResults $trajectorySingleToolUseResults)
  {
    $this->trajectorySingleToolUseResults = $trajectorySingleToolUseResults;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectorySingleToolUseResults
   */
  public function getTrajectorySingleToolUseResults()
  {
    return $this->trajectorySingleToolUseResults;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1EvaluateInstancesResponse::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1EvaluateInstancesResponse');
