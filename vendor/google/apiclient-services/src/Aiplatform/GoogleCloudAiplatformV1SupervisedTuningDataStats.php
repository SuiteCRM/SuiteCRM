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

class GoogleCloudAiplatformV1SupervisedTuningDataStats extends \Google\Collection
{
  protected $collection_key = 'userDatasetExamples';
  /**
   * @var string[]
   */
  public $droppedExampleReasons;
  /**
   * @var string
   */
  public $totalBillableCharacterCount;
  /**
   * @var string
   */
  public $totalBillableTokenCount;
  /**
   * @var string
   */
  public $totalTruncatedExampleCount;
  /**
   * @var string
   */
  public $totalTuningCharacterCount;
  /**
   * @var string[]
   */
  public $truncatedExampleIndices;
  /**
   * @var string
   */
  public $tuningDatasetExampleCount;
  /**
   * @var string
   */
  public $tuningStepCount;
  protected $userDatasetExamplesType = GoogleCloudAiplatformV1Content::class;
  protected $userDatasetExamplesDataType = 'array';
  protected $userInputTokenDistributionType = GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution::class;
  protected $userInputTokenDistributionDataType = '';
  protected $userMessagePerExampleDistributionType = GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution::class;
  protected $userMessagePerExampleDistributionDataType = '';
  protected $userOutputTokenDistributionType = GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution::class;
  protected $userOutputTokenDistributionDataType = '';

  /**
   * @param string[]
   */
  public function setDroppedExampleReasons($droppedExampleReasons)
  {
    $this->droppedExampleReasons = $droppedExampleReasons;
  }
  /**
   * @return string[]
   */
  public function getDroppedExampleReasons()
  {
    return $this->droppedExampleReasons;
  }
  /**
   * @param string
   */
  public function setTotalBillableCharacterCount($totalBillableCharacterCount)
  {
    $this->totalBillableCharacterCount = $totalBillableCharacterCount;
  }
  /**
   * @return string
   */
  public function getTotalBillableCharacterCount()
  {
    return $this->totalBillableCharacterCount;
  }
  /**
   * @param string
   */
  public function setTotalBillableTokenCount($totalBillableTokenCount)
  {
    $this->totalBillableTokenCount = $totalBillableTokenCount;
  }
  /**
   * @return string
   */
  public function getTotalBillableTokenCount()
  {
    return $this->totalBillableTokenCount;
  }
  /**
   * @param string
   */
  public function setTotalTruncatedExampleCount($totalTruncatedExampleCount)
  {
    $this->totalTruncatedExampleCount = $totalTruncatedExampleCount;
  }
  /**
   * @return string
   */
  public function getTotalTruncatedExampleCount()
  {
    return $this->totalTruncatedExampleCount;
  }
  /**
   * @param string
   */
  public function setTotalTuningCharacterCount($totalTuningCharacterCount)
  {
    $this->totalTuningCharacterCount = $totalTuningCharacterCount;
  }
  /**
   * @return string
   */
  public function getTotalTuningCharacterCount()
  {
    return $this->totalTuningCharacterCount;
  }
  /**
   * @param string[]
   */
  public function setTruncatedExampleIndices($truncatedExampleIndices)
  {
    $this->truncatedExampleIndices = $truncatedExampleIndices;
  }
  /**
   * @return string[]
   */
  public function getTruncatedExampleIndices()
  {
    return $this->truncatedExampleIndices;
  }
  /**
   * @param string
   */
  public function setTuningDatasetExampleCount($tuningDatasetExampleCount)
  {
    $this->tuningDatasetExampleCount = $tuningDatasetExampleCount;
  }
  /**
   * @return string
   */
  public function getTuningDatasetExampleCount()
  {
    return $this->tuningDatasetExampleCount;
  }
  /**
   * @param string
   */
  public function setTuningStepCount($tuningStepCount)
  {
    $this->tuningStepCount = $tuningStepCount;
  }
  /**
   * @return string
   */
  public function getTuningStepCount()
  {
    return $this->tuningStepCount;
  }
  /**
   * @param GoogleCloudAiplatformV1Content[]
   */
  public function setUserDatasetExamples($userDatasetExamples)
  {
    $this->userDatasetExamples = $userDatasetExamples;
  }
  /**
   * @return GoogleCloudAiplatformV1Content[]
   */
  public function getUserDatasetExamples()
  {
    return $this->userDatasetExamples;
  }
  /**
   * @param GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution
   */
  public function setUserInputTokenDistribution(GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution $userInputTokenDistribution)
  {
    $this->userInputTokenDistribution = $userInputTokenDistribution;
  }
  /**
   * @return GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution
   */
  public function getUserInputTokenDistribution()
  {
    return $this->userInputTokenDistribution;
  }
  /**
   * @param GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution
   */
  public function setUserMessagePerExampleDistribution(GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution $userMessagePerExampleDistribution)
  {
    $this->userMessagePerExampleDistribution = $userMessagePerExampleDistribution;
  }
  /**
   * @return GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution
   */
  public function getUserMessagePerExampleDistribution()
  {
    return $this->userMessagePerExampleDistribution;
  }
  /**
   * @param GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution
   */
  public function setUserOutputTokenDistribution(GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution $userOutputTokenDistribution)
  {
    $this->userOutputTokenDistribution = $userOutputTokenDistribution;
  }
  /**
   * @return GoogleCloudAiplatformV1SupervisedTuningDatasetDistribution
   */
  public function getUserOutputTokenDistribution()
  {
    return $this->userOutputTokenDistribution;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SupervisedTuningDataStats::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SupervisedTuningDataStats');
