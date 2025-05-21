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

class GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpec extends \Google\Model
{
  protected $metricType = GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecMetricSpec::class;
  protected $metricDataType = '';
  /**
   * @var string
   */
  public $multiTrialAlgorithm;
  protected $searchTrialSpecType = GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecSearchTrialSpec::class;
  protected $searchTrialSpecDataType = '';
  protected $trainTrialSpecType = GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecTrainTrialSpec::class;
  protected $trainTrialSpecDataType = '';

  /**
   * @param GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecMetricSpec
   */
  public function setMetric(GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecMetricSpec $metric)
  {
    $this->metric = $metric;
  }
  /**
   * @return GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecMetricSpec
   */
  public function getMetric()
  {
    return $this->metric;
  }
  /**
   * @param string
   */
  public function setMultiTrialAlgorithm($multiTrialAlgorithm)
  {
    $this->multiTrialAlgorithm = $multiTrialAlgorithm;
  }
  /**
   * @return string
   */
  public function getMultiTrialAlgorithm()
  {
    return $this->multiTrialAlgorithm;
  }
  /**
   * @param GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecSearchTrialSpec
   */
  public function setSearchTrialSpec(GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecSearchTrialSpec $searchTrialSpec)
  {
    $this->searchTrialSpec = $searchTrialSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecSearchTrialSpec
   */
  public function getSearchTrialSpec()
  {
    return $this->searchTrialSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecTrainTrialSpec
   */
  public function setTrainTrialSpec(GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecTrainTrialSpec $trainTrialSpec)
  {
    $this->trainTrialSpec = $trainTrialSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecTrainTrialSpec
   */
  public function getTrainTrialSpec()
  {
    return $this->trainTrialSpec;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpec');
