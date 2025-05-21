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

class GoogleCloudAiplatformV1StudySpec extends \Google\Collection
{
  protected $collection_key = 'parameters';
  /**
   * @var string
   */
  public $algorithm;
  protected $convexAutomatedStoppingSpecType = GoogleCloudAiplatformV1StudySpecConvexAutomatedStoppingSpec::class;
  protected $convexAutomatedStoppingSpecDataType = '';
  protected $decayCurveStoppingSpecType = GoogleCloudAiplatformV1StudySpecDecayCurveAutomatedStoppingSpec::class;
  protected $decayCurveStoppingSpecDataType = '';
  /**
   * @var string
   */
  public $measurementSelectionType;
  protected $medianAutomatedStoppingSpecType = GoogleCloudAiplatformV1StudySpecMedianAutomatedStoppingSpec::class;
  protected $medianAutomatedStoppingSpecDataType = '';
  protected $metricsType = GoogleCloudAiplatformV1StudySpecMetricSpec::class;
  protected $metricsDataType = 'array';
  /**
   * @var string
   */
  public $observationNoise;
  protected $parametersType = GoogleCloudAiplatformV1StudySpecParameterSpec::class;
  protected $parametersDataType = 'array';
  protected $studyStoppingConfigType = GoogleCloudAiplatformV1StudySpecStudyStoppingConfig::class;
  protected $studyStoppingConfigDataType = '';

  /**
   * @param string
   */
  public function setAlgorithm($algorithm)
  {
    $this->algorithm = $algorithm;
  }
  /**
   * @return string
   */
  public function getAlgorithm()
  {
    return $this->algorithm;
  }
  /**
   * @param GoogleCloudAiplatformV1StudySpecConvexAutomatedStoppingSpec
   */
  public function setConvexAutomatedStoppingSpec(GoogleCloudAiplatformV1StudySpecConvexAutomatedStoppingSpec $convexAutomatedStoppingSpec)
  {
    $this->convexAutomatedStoppingSpec = $convexAutomatedStoppingSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecConvexAutomatedStoppingSpec
   */
  public function getConvexAutomatedStoppingSpec()
  {
    return $this->convexAutomatedStoppingSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1StudySpecDecayCurveAutomatedStoppingSpec
   */
  public function setDecayCurveStoppingSpec(GoogleCloudAiplatformV1StudySpecDecayCurveAutomatedStoppingSpec $decayCurveStoppingSpec)
  {
    $this->decayCurveStoppingSpec = $decayCurveStoppingSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecDecayCurveAutomatedStoppingSpec
   */
  public function getDecayCurveStoppingSpec()
  {
    return $this->decayCurveStoppingSpec;
  }
  /**
   * @param string
   */
  public function setMeasurementSelectionType($measurementSelectionType)
  {
    $this->measurementSelectionType = $measurementSelectionType;
  }
  /**
   * @return string
   */
  public function getMeasurementSelectionType()
  {
    return $this->measurementSelectionType;
  }
  /**
   * @param GoogleCloudAiplatformV1StudySpecMedianAutomatedStoppingSpec
   */
  public function setMedianAutomatedStoppingSpec(GoogleCloudAiplatformV1StudySpecMedianAutomatedStoppingSpec $medianAutomatedStoppingSpec)
  {
    $this->medianAutomatedStoppingSpec = $medianAutomatedStoppingSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecMedianAutomatedStoppingSpec
   */
  public function getMedianAutomatedStoppingSpec()
  {
    return $this->medianAutomatedStoppingSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1StudySpecMetricSpec[]
   */
  public function setMetrics($metrics)
  {
    $this->metrics = $metrics;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecMetricSpec[]
   */
  public function getMetrics()
  {
    return $this->metrics;
  }
  /**
   * @param string
   */
  public function setObservationNoise($observationNoise)
  {
    $this->observationNoise = $observationNoise;
  }
  /**
   * @return string
   */
  public function getObservationNoise()
  {
    return $this->observationNoise;
  }
  /**
   * @param GoogleCloudAiplatformV1StudySpecParameterSpec[]
   */
  public function setParameters($parameters)
  {
    $this->parameters = $parameters;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecParameterSpec[]
   */
  public function getParameters()
  {
    return $this->parameters;
  }
  /**
   * @param GoogleCloudAiplatformV1StudySpecStudyStoppingConfig
   */
  public function setStudyStoppingConfig(GoogleCloudAiplatformV1StudySpecStudyStoppingConfig $studyStoppingConfig)
  {
    $this->studyStoppingConfig = $studyStoppingConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecStudyStoppingConfig
   */
  public function getStudyStoppingConfig()
  {
    return $this->studyStoppingConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1StudySpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1StudySpec');
