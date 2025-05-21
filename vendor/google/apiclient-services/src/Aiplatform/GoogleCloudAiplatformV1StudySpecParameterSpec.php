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

class GoogleCloudAiplatformV1StudySpecParameterSpec extends \Google\Collection
{
  protected $collection_key = 'conditionalParameterSpecs';
  protected $categoricalValueSpecType = GoogleCloudAiplatformV1StudySpecParameterSpecCategoricalValueSpec::class;
  protected $categoricalValueSpecDataType = '';
  protected $conditionalParameterSpecsType = GoogleCloudAiplatformV1StudySpecParameterSpecConditionalParameterSpec::class;
  protected $conditionalParameterSpecsDataType = 'array';
  protected $discreteValueSpecType = GoogleCloudAiplatformV1StudySpecParameterSpecDiscreteValueSpec::class;
  protected $discreteValueSpecDataType = '';
  protected $doubleValueSpecType = GoogleCloudAiplatformV1StudySpecParameterSpecDoubleValueSpec::class;
  protected $doubleValueSpecDataType = '';
  protected $integerValueSpecType = GoogleCloudAiplatformV1StudySpecParameterSpecIntegerValueSpec::class;
  protected $integerValueSpecDataType = '';
  /**
   * @var string
   */
  public $parameterId;
  /**
   * @var string
   */
  public $scaleType;

  /**
   * @param GoogleCloudAiplatformV1StudySpecParameterSpecCategoricalValueSpec
   */
  public function setCategoricalValueSpec(GoogleCloudAiplatformV1StudySpecParameterSpecCategoricalValueSpec $categoricalValueSpec)
  {
    $this->categoricalValueSpec = $categoricalValueSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecParameterSpecCategoricalValueSpec
   */
  public function getCategoricalValueSpec()
  {
    return $this->categoricalValueSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1StudySpecParameterSpecConditionalParameterSpec[]
   */
  public function setConditionalParameterSpecs($conditionalParameterSpecs)
  {
    $this->conditionalParameterSpecs = $conditionalParameterSpecs;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecParameterSpecConditionalParameterSpec[]
   */
  public function getConditionalParameterSpecs()
  {
    return $this->conditionalParameterSpecs;
  }
  /**
   * @param GoogleCloudAiplatformV1StudySpecParameterSpecDiscreteValueSpec
   */
  public function setDiscreteValueSpec(GoogleCloudAiplatformV1StudySpecParameterSpecDiscreteValueSpec $discreteValueSpec)
  {
    $this->discreteValueSpec = $discreteValueSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecParameterSpecDiscreteValueSpec
   */
  public function getDiscreteValueSpec()
  {
    return $this->discreteValueSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1StudySpecParameterSpecDoubleValueSpec
   */
  public function setDoubleValueSpec(GoogleCloudAiplatformV1StudySpecParameterSpecDoubleValueSpec $doubleValueSpec)
  {
    $this->doubleValueSpec = $doubleValueSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecParameterSpecDoubleValueSpec
   */
  public function getDoubleValueSpec()
  {
    return $this->doubleValueSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1StudySpecParameterSpecIntegerValueSpec
   */
  public function setIntegerValueSpec(GoogleCloudAiplatformV1StudySpecParameterSpecIntegerValueSpec $integerValueSpec)
  {
    $this->integerValueSpec = $integerValueSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecParameterSpecIntegerValueSpec
   */
  public function getIntegerValueSpec()
  {
    return $this->integerValueSpec;
  }
  /**
   * @param string
   */
  public function setParameterId($parameterId)
  {
    $this->parameterId = $parameterId;
  }
  /**
   * @return string
   */
  public function getParameterId()
  {
    return $this->parameterId;
  }
  /**
   * @param string
   */
  public function setScaleType($scaleType)
  {
    $this->scaleType = $scaleType;
  }
  /**
   * @return string
   */
  public function getScaleType()
  {
    return $this->scaleType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1StudySpecParameterSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1StudySpecParameterSpec');
