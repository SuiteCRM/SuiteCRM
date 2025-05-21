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

class GoogleCloudAiplatformV1EvaluateInstancesRequest extends \Google\Model
{
  protected $autoraterConfigType = GoogleCloudAiplatformV1AutoraterConfig::class;
  protected $autoraterConfigDataType = '';
  protected $bleuInputType = GoogleCloudAiplatformV1BleuInput::class;
  protected $bleuInputDataType = '';
  protected $coherenceInputType = GoogleCloudAiplatformV1CoherenceInput::class;
  protected $coherenceInputDataType = '';
  protected $cometInputType = GoogleCloudAiplatformV1CometInput::class;
  protected $cometInputDataType = '';
  protected $exactMatchInputType = GoogleCloudAiplatformV1ExactMatchInput::class;
  protected $exactMatchInputDataType = '';
  protected $fluencyInputType = GoogleCloudAiplatformV1FluencyInput::class;
  protected $fluencyInputDataType = '';
  protected $fulfillmentInputType = GoogleCloudAiplatformV1FulfillmentInput::class;
  protected $fulfillmentInputDataType = '';
  protected $groundednessInputType = GoogleCloudAiplatformV1GroundednessInput::class;
  protected $groundednessInputDataType = '';
  protected $metricxInputType = GoogleCloudAiplatformV1MetricxInput::class;
  protected $metricxInputDataType = '';
  protected $pairwiseMetricInputType = GoogleCloudAiplatformV1PairwiseMetricInput::class;
  protected $pairwiseMetricInputDataType = '';
  protected $pairwiseQuestionAnsweringQualityInputType = GoogleCloudAiplatformV1PairwiseQuestionAnsweringQualityInput::class;
  protected $pairwiseQuestionAnsweringQualityInputDataType = '';
  protected $pairwiseSummarizationQualityInputType = GoogleCloudAiplatformV1PairwiseSummarizationQualityInput::class;
  protected $pairwiseSummarizationQualityInputDataType = '';
  protected $pointwiseMetricInputType = GoogleCloudAiplatformV1PointwiseMetricInput::class;
  protected $pointwiseMetricInputDataType = '';
  protected $questionAnsweringCorrectnessInputType = GoogleCloudAiplatformV1QuestionAnsweringCorrectnessInput::class;
  protected $questionAnsweringCorrectnessInputDataType = '';
  protected $questionAnsweringHelpfulnessInputType = GoogleCloudAiplatformV1QuestionAnsweringHelpfulnessInput::class;
  protected $questionAnsweringHelpfulnessInputDataType = '';
  protected $questionAnsweringQualityInputType = GoogleCloudAiplatformV1QuestionAnsweringQualityInput::class;
  protected $questionAnsweringQualityInputDataType = '';
  protected $questionAnsweringRelevanceInputType = GoogleCloudAiplatformV1QuestionAnsweringRelevanceInput::class;
  protected $questionAnsweringRelevanceInputDataType = '';
  protected $rougeInputType = GoogleCloudAiplatformV1RougeInput::class;
  protected $rougeInputDataType = '';
  protected $safetyInputType = GoogleCloudAiplatformV1SafetyInput::class;
  protected $safetyInputDataType = '';
  protected $summarizationHelpfulnessInputType = GoogleCloudAiplatformV1SummarizationHelpfulnessInput::class;
  protected $summarizationHelpfulnessInputDataType = '';
  protected $summarizationQualityInputType = GoogleCloudAiplatformV1SummarizationQualityInput::class;
  protected $summarizationQualityInputDataType = '';
  protected $summarizationVerbosityInputType = GoogleCloudAiplatformV1SummarizationVerbosityInput::class;
  protected $summarizationVerbosityInputDataType = '';
  protected $toolCallValidInputType = GoogleCloudAiplatformV1ToolCallValidInput::class;
  protected $toolCallValidInputDataType = '';
  protected $toolNameMatchInputType = GoogleCloudAiplatformV1ToolNameMatchInput::class;
  protected $toolNameMatchInputDataType = '';
  protected $toolParameterKeyMatchInputType = GoogleCloudAiplatformV1ToolParameterKeyMatchInput::class;
  protected $toolParameterKeyMatchInputDataType = '';
  protected $toolParameterKvMatchInputType = GoogleCloudAiplatformV1ToolParameterKVMatchInput::class;
  protected $toolParameterKvMatchInputDataType = '';
  protected $trajectoryAnyOrderMatchInputType = GoogleCloudAiplatformV1TrajectoryAnyOrderMatchInput::class;
  protected $trajectoryAnyOrderMatchInputDataType = '';
  protected $trajectoryExactMatchInputType = GoogleCloudAiplatformV1TrajectoryExactMatchInput::class;
  protected $trajectoryExactMatchInputDataType = '';
  protected $trajectoryInOrderMatchInputType = GoogleCloudAiplatformV1TrajectoryInOrderMatchInput::class;
  protected $trajectoryInOrderMatchInputDataType = '';
  protected $trajectoryPrecisionInputType = GoogleCloudAiplatformV1TrajectoryPrecisionInput::class;
  protected $trajectoryPrecisionInputDataType = '';
  protected $trajectoryRecallInputType = GoogleCloudAiplatformV1TrajectoryRecallInput::class;
  protected $trajectoryRecallInputDataType = '';
  protected $trajectorySingleToolUseInputType = GoogleCloudAiplatformV1TrajectorySingleToolUseInput::class;
  protected $trajectorySingleToolUseInputDataType = '';

  /**
   * @param GoogleCloudAiplatformV1AutoraterConfig
   */
  public function setAutoraterConfig(GoogleCloudAiplatformV1AutoraterConfig $autoraterConfig)
  {
    $this->autoraterConfig = $autoraterConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1AutoraterConfig
   */
  public function getAutoraterConfig()
  {
    return $this->autoraterConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1BleuInput
   */
  public function setBleuInput(GoogleCloudAiplatformV1BleuInput $bleuInput)
  {
    $this->bleuInput = $bleuInput;
  }
  /**
   * @return GoogleCloudAiplatformV1BleuInput
   */
  public function getBleuInput()
  {
    return $this->bleuInput;
  }
  /**
   * @param GoogleCloudAiplatformV1CoherenceInput
   */
  public function setCoherenceInput(GoogleCloudAiplatformV1CoherenceInput $coherenceInput)
  {
    $this->coherenceInput = $coherenceInput;
  }
  /**
   * @return GoogleCloudAiplatformV1CoherenceInput
   */
  public function getCoherenceInput()
  {
    return $this->coherenceInput;
  }
  /**
   * @param GoogleCloudAiplatformV1CometInput
   */
  public function setCometInput(GoogleCloudAiplatformV1CometInput $cometInput)
  {
    $this->cometInput = $cometInput;
  }
  /**
   * @return GoogleCloudAiplatformV1CometInput
   */
  public function getCometInput()
  {
    return $this->cometInput;
  }
  /**
   * @param GoogleCloudAiplatformV1ExactMatchInput
   */
  public function setExactMatchInput(GoogleCloudAiplatformV1ExactMatchInput $exactMatchInput)
  {
    $this->exactMatchInput = $exactMatchInput;
  }
  /**
   * @return GoogleCloudAiplatformV1ExactMatchInput
   */
  public function getExactMatchInput()
  {
    return $this->exactMatchInput;
  }
  /**
   * @param GoogleCloudAiplatformV1FluencyInput
   */
  public function setFluencyInput(GoogleCloudAiplatformV1FluencyInput $fluencyInput)
  {
    $this->fluencyInput = $fluencyInput;
  }
  /**
   * @return GoogleCloudAiplatformV1FluencyInput
   */
  public function getFluencyInput()
  {
    return $this->fluencyInput;
  }
  /**
   * @param GoogleCloudAiplatformV1FulfillmentInput
   */
  public function setFulfillmentInput(GoogleCloudAiplatformV1FulfillmentInput $fulfillmentInput)
  {
    $this->fulfillmentInput = $fulfillmentInput;
  }
  /**
   * @return GoogleCloudAiplatformV1FulfillmentInput
   */
  public function getFulfillmentInput()
  {
    return $this->fulfillmentInput;
  }
  /**
   * @param GoogleCloudAiplatformV1GroundednessInput
   */
  public function setGroundednessInput(GoogleCloudAiplatformV1GroundednessInput $groundednessInput)
  {
    $this->groundednessInput = $groundednessInput;
  }
  /**
   * @return GoogleCloudAiplatformV1GroundednessInput
   */
  public function getGroundednessInput()
  {
    return $this->groundednessInput;
  }
  /**
   * @param GoogleCloudAiplatformV1MetricxInput
   */
  public function setMetricxInput(GoogleCloudAiplatformV1MetricxInput $metricxInput)
  {
    $this->metricxInput = $metricxInput;
  }
  /**
   * @return GoogleCloudAiplatformV1MetricxInput
   */
  public function getMetricxInput()
  {
    return $this->metricxInput;
  }
  /**
   * @param GoogleCloudAiplatformV1PairwiseMetricInput
   */
  public function setPairwiseMetricInput(GoogleCloudAiplatformV1PairwiseMetricInput $pairwiseMetricInput)
  {
    $this->pairwiseMetricInput = $pairwiseMetricInput;
  }
  /**
   * @return GoogleCloudAiplatformV1PairwiseMetricInput
   */
  public function getPairwiseMetricInput()
  {
    return $this->pairwiseMetricInput;
  }
  /**
   * @param GoogleCloudAiplatformV1PairwiseQuestionAnsweringQualityInput
   */
  public function setPairwiseQuestionAnsweringQualityInput(GoogleCloudAiplatformV1PairwiseQuestionAnsweringQualityInput $pairwiseQuestionAnsweringQualityInput)
  {
    $this->pairwiseQuestionAnsweringQualityInput = $pairwiseQuestionAnsweringQualityInput;
  }
  /**
   * @return GoogleCloudAiplatformV1PairwiseQuestionAnsweringQualityInput
   */
  public function getPairwiseQuestionAnsweringQualityInput()
  {
    return $this->pairwiseQuestionAnsweringQualityInput;
  }
  /**
   * @param GoogleCloudAiplatformV1PairwiseSummarizationQualityInput
   */
  public function setPairwiseSummarizationQualityInput(GoogleCloudAiplatformV1PairwiseSummarizationQualityInput $pairwiseSummarizationQualityInput)
  {
    $this->pairwiseSummarizationQualityInput = $pairwiseSummarizationQualityInput;
  }
  /**
   * @return GoogleCloudAiplatformV1PairwiseSummarizationQualityInput
   */
  public function getPairwiseSummarizationQualityInput()
  {
    return $this->pairwiseSummarizationQualityInput;
  }
  /**
   * @param GoogleCloudAiplatformV1PointwiseMetricInput
   */
  public function setPointwiseMetricInput(GoogleCloudAiplatformV1PointwiseMetricInput $pointwiseMetricInput)
  {
    $this->pointwiseMetricInput = $pointwiseMetricInput;
  }
  /**
   * @return GoogleCloudAiplatformV1PointwiseMetricInput
   */
  public function getPointwiseMetricInput()
  {
    return $this->pointwiseMetricInput;
  }
  /**
   * @param GoogleCloudAiplatformV1QuestionAnsweringCorrectnessInput
   */
  public function setQuestionAnsweringCorrectnessInput(GoogleCloudAiplatformV1QuestionAnsweringCorrectnessInput $questionAnsweringCorrectnessInput)
  {
    $this->questionAnsweringCorrectnessInput = $questionAnsweringCorrectnessInput;
  }
  /**
   * @return GoogleCloudAiplatformV1QuestionAnsweringCorrectnessInput
   */
  public function getQuestionAnsweringCorrectnessInput()
  {
    return $this->questionAnsweringCorrectnessInput;
  }
  /**
   * @param GoogleCloudAiplatformV1QuestionAnsweringHelpfulnessInput
   */
  public function setQuestionAnsweringHelpfulnessInput(GoogleCloudAiplatformV1QuestionAnsweringHelpfulnessInput $questionAnsweringHelpfulnessInput)
  {
    $this->questionAnsweringHelpfulnessInput = $questionAnsweringHelpfulnessInput;
  }
  /**
   * @return GoogleCloudAiplatformV1QuestionAnsweringHelpfulnessInput
   */
  public function getQuestionAnsweringHelpfulnessInput()
  {
    return $this->questionAnsweringHelpfulnessInput;
  }
  /**
   * @param GoogleCloudAiplatformV1QuestionAnsweringQualityInput
   */
  public function setQuestionAnsweringQualityInput(GoogleCloudAiplatformV1QuestionAnsweringQualityInput $questionAnsweringQualityInput)
  {
    $this->questionAnsweringQualityInput = $questionAnsweringQualityInput;
  }
  /**
   * @return GoogleCloudAiplatformV1QuestionAnsweringQualityInput
   */
  public function getQuestionAnsweringQualityInput()
  {
    return $this->questionAnsweringQualityInput;
  }
  /**
   * @param GoogleCloudAiplatformV1QuestionAnsweringRelevanceInput
   */
  public function setQuestionAnsweringRelevanceInput(GoogleCloudAiplatformV1QuestionAnsweringRelevanceInput $questionAnsweringRelevanceInput)
  {
    $this->questionAnsweringRelevanceInput = $questionAnsweringRelevanceInput;
  }
  /**
   * @return GoogleCloudAiplatformV1QuestionAnsweringRelevanceInput
   */
  public function getQuestionAnsweringRelevanceInput()
  {
    return $this->questionAnsweringRelevanceInput;
  }
  /**
   * @param GoogleCloudAiplatformV1RougeInput
   */
  public function setRougeInput(GoogleCloudAiplatformV1RougeInput $rougeInput)
  {
    $this->rougeInput = $rougeInput;
  }
  /**
   * @return GoogleCloudAiplatformV1RougeInput
   */
  public function getRougeInput()
  {
    return $this->rougeInput;
  }
  /**
   * @param GoogleCloudAiplatformV1SafetyInput
   */
  public function setSafetyInput(GoogleCloudAiplatformV1SafetyInput $safetyInput)
  {
    $this->safetyInput = $safetyInput;
  }
  /**
   * @return GoogleCloudAiplatformV1SafetyInput
   */
  public function getSafetyInput()
  {
    return $this->safetyInput;
  }
  /**
   * @param GoogleCloudAiplatformV1SummarizationHelpfulnessInput
   */
  public function setSummarizationHelpfulnessInput(GoogleCloudAiplatformV1SummarizationHelpfulnessInput $summarizationHelpfulnessInput)
  {
    $this->summarizationHelpfulnessInput = $summarizationHelpfulnessInput;
  }
  /**
   * @return GoogleCloudAiplatformV1SummarizationHelpfulnessInput
   */
  public function getSummarizationHelpfulnessInput()
  {
    return $this->summarizationHelpfulnessInput;
  }
  /**
   * @param GoogleCloudAiplatformV1SummarizationQualityInput
   */
  public function setSummarizationQualityInput(GoogleCloudAiplatformV1SummarizationQualityInput $summarizationQualityInput)
  {
    $this->summarizationQualityInput = $summarizationQualityInput;
  }
  /**
   * @return GoogleCloudAiplatformV1SummarizationQualityInput
   */
  public function getSummarizationQualityInput()
  {
    return $this->summarizationQualityInput;
  }
  /**
   * @param GoogleCloudAiplatformV1SummarizationVerbosityInput
   */
  public function setSummarizationVerbosityInput(GoogleCloudAiplatformV1SummarizationVerbosityInput $summarizationVerbosityInput)
  {
    $this->summarizationVerbosityInput = $summarizationVerbosityInput;
  }
  /**
   * @return GoogleCloudAiplatformV1SummarizationVerbosityInput
   */
  public function getSummarizationVerbosityInput()
  {
    return $this->summarizationVerbosityInput;
  }
  /**
   * @param GoogleCloudAiplatformV1ToolCallValidInput
   */
  public function setToolCallValidInput(GoogleCloudAiplatformV1ToolCallValidInput $toolCallValidInput)
  {
    $this->toolCallValidInput = $toolCallValidInput;
  }
  /**
   * @return GoogleCloudAiplatformV1ToolCallValidInput
   */
  public function getToolCallValidInput()
  {
    return $this->toolCallValidInput;
  }
  /**
   * @param GoogleCloudAiplatformV1ToolNameMatchInput
   */
  public function setToolNameMatchInput(GoogleCloudAiplatformV1ToolNameMatchInput $toolNameMatchInput)
  {
    $this->toolNameMatchInput = $toolNameMatchInput;
  }
  /**
   * @return GoogleCloudAiplatformV1ToolNameMatchInput
   */
  public function getToolNameMatchInput()
  {
    return $this->toolNameMatchInput;
  }
  /**
   * @param GoogleCloudAiplatformV1ToolParameterKeyMatchInput
   */
  public function setToolParameterKeyMatchInput(GoogleCloudAiplatformV1ToolParameterKeyMatchInput $toolParameterKeyMatchInput)
  {
    $this->toolParameterKeyMatchInput = $toolParameterKeyMatchInput;
  }
  /**
   * @return GoogleCloudAiplatformV1ToolParameterKeyMatchInput
   */
  public function getToolParameterKeyMatchInput()
  {
    return $this->toolParameterKeyMatchInput;
  }
  /**
   * @param GoogleCloudAiplatformV1ToolParameterKVMatchInput
   */
  public function setToolParameterKvMatchInput(GoogleCloudAiplatformV1ToolParameterKVMatchInput $toolParameterKvMatchInput)
  {
    $this->toolParameterKvMatchInput = $toolParameterKvMatchInput;
  }
  /**
   * @return GoogleCloudAiplatformV1ToolParameterKVMatchInput
   */
  public function getToolParameterKvMatchInput()
  {
    return $this->toolParameterKvMatchInput;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectoryAnyOrderMatchInput
   */
  public function setTrajectoryAnyOrderMatchInput(GoogleCloudAiplatformV1TrajectoryAnyOrderMatchInput $trajectoryAnyOrderMatchInput)
  {
    $this->trajectoryAnyOrderMatchInput = $trajectoryAnyOrderMatchInput;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectoryAnyOrderMatchInput
   */
  public function getTrajectoryAnyOrderMatchInput()
  {
    return $this->trajectoryAnyOrderMatchInput;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectoryExactMatchInput
   */
  public function setTrajectoryExactMatchInput(GoogleCloudAiplatformV1TrajectoryExactMatchInput $trajectoryExactMatchInput)
  {
    $this->trajectoryExactMatchInput = $trajectoryExactMatchInput;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectoryExactMatchInput
   */
  public function getTrajectoryExactMatchInput()
  {
    return $this->trajectoryExactMatchInput;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectoryInOrderMatchInput
   */
  public function setTrajectoryInOrderMatchInput(GoogleCloudAiplatformV1TrajectoryInOrderMatchInput $trajectoryInOrderMatchInput)
  {
    $this->trajectoryInOrderMatchInput = $trajectoryInOrderMatchInput;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectoryInOrderMatchInput
   */
  public function getTrajectoryInOrderMatchInput()
  {
    return $this->trajectoryInOrderMatchInput;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectoryPrecisionInput
   */
  public function setTrajectoryPrecisionInput(GoogleCloudAiplatformV1TrajectoryPrecisionInput $trajectoryPrecisionInput)
  {
    $this->trajectoryPrecisionInput = $trajectoryPrecisionInput;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectoryPrecisionInput
   */
  public function getTrajectoryPrecisionInput()
  {
    return $this->trajectoryPrecisionInput;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectoryRecallInput
   */
  public function setTrajectoryRecallInput(GoogleCloudAiplatformV1TrajectoryRecallInput $trajectoryRecallInput)
  {
    $this->trajectoryRecallInput = $trajectoryRecallInput;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectoryRecallInput
   */
  public function getTrajectoryRecallInput()
  {
    return $this->trajectoryRecallInput;
  }
  /**
   * @param GoogleCloudAiplatformV1TrajectorySingleToolUseInput
   */
  public function setTrajectorySingleToolUseInput(GoogleCloudAiplatformV1TrajectorySingleToolUseInput $trajectorySingleToolUseInput)
  {
    $this->trajectorySingleToolUseInput = $trajectorySingleToolUseInput;
  }
  /**
   * @return GoogleCloudAiplatformV1TrajectorySingleToolUseInput
   */
  public function getTrajectorySingleToolUseInput()
  {
    return $this->trajectorySingleToolUseInput;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1EvaluateInstancesRequest::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1EvaluateInstancesRequest');
