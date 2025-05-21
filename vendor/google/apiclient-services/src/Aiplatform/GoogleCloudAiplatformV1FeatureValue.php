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

class GoogleCloudAiplatformV1FeatureValue extends \Google\Model
{
  protected $boolArrayValueType = GoogleCloudAiplatformV1BoolArray::class;
  protected $boolArrayValueDataType = '';
  /**
   * @var bool
   */
  public $boolValue;
  /**
   * @var string
   */
  public $bytesValue;
  protected $doubleArrayValueType = GoogleCloudAiplatformV1DoubleArray::class;
  protected $doubleArrayValueDataType = '';
  public $doubleValue;
  protected $int64ArrayValueType = GoogleCloudAiplatformV1Int64Array::class;
  protected $int64ArrayValueDataType = '';
  /**
   * @var string
   */
  public $int64Value;
  protected $metadataType = GoogleCloudAiplatformV1FeatureValueMetadata::class;
  protected $metadataDataType = '';
  protected $stringArrayValueType = GoogleCloudAiplatformV1StringArray::class;
  protected $stringArrayValueDataType = '';
  /**
   * @var string
   */
  public $stringValue;
  protected $structValueType = GoogleCloudAiplatformV1StructValue::class;
  protected $structValueDataType = '';

  /**
   * @param GoogleCloudAiplatformV1BoolArray
   */
  public function setBoolArrayValue(GoogleCloudAiplatformV1BoolArray $boolArrayValue)
  {
    $this->boolArrayValue = $boolArrayValue;
  }
  /**
   * @return GoogleCloudAiplatformV1BoolArray
   */
  public function getBoolArrayValue()
  {
    return $this->boolArrayValue;
  }
  /**
   * @param bool
   */
  public function setBoolValue($boolValue)
  {
    $this->boolValue = $boolValue;
  }
  /**
   * @return bool
   */
  public function getBoolValue()
  {
    return $this->boolValue;
  }
  /**
   * @param string
   */
  public function setBytesValue($bytesValue)
  {
    $this->bytesValue = $bytesValue;
  }
  /**
   * @return string
   */
  public function getBytesValue()
  {
    return $this->bytesValue;
  }
  /**
   * @param GoogleCloudAiplatformV1DoubleArray
   */
  public function setDoubleArrayValue(GoogleCloudAiplatformV1DoubleArray $doubleArrayValue)
  {
    $this->doubleArrayValue = $doubleArrayValue;
  }
  /**
   * @return GoogleCloudAiplatformV1DoubleArray
   */
  public function getDoubleArrayValue()
  {
    return $this->doubleArrayValue;
  }
  public function setDoubleValue($doubleValue)
  {
    $this->doubleValue = $doubleValue;
  }
  public function getDoubleValue()
  {
    return $this->doubleValue;
  }
  /**
   * @param GoogleCloudAiplatformV1Int64Array
   */
  public function setInt64ArrayValue(GoogleCloudAiplatformV1Int64Array $int64ArrayValue)
  {
    $this->int64ArrayValue = $int64ArrayValue;
  }
  /**
   * @return GoogleCloudAiplatformV1Int64Array
   */
  public function getInt64ArrayValue()
  {
    return $this->int64ArrayValue;
  }
  /**
   * @param string
   */
  public function setInt64Value($int64Value)
  {
    $this->int64Value = $int64Value;
  }
  /**
   * @return string
   */
  public function getInt64Value()
  {
    return $this->int64Value;
  }
  /**
   * @param GoogleCloudAiplatformV1FeatureValueMetadata
   */
  public function setMetadata(GoogleCloudAiplatformV1FeatureValueMetadata $metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureValueMetadata
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
  /**
   * @param GoogleCloudAiplatformV1StringArray
   */
  public function setStringArrayValue(GoogleCloudAiplatformV1StringArray $stringArrayValue)
  {
    $this->stringArrayValue = $stringArrayValue;
  }
  /**
   * @return GoogleCloudAiplatformV1StringArray
   */
  public function getStringArrayValue()
  {
    return $this->stringArrayValue;
  }
  /**
   * @param string
   */
  public function setStringValue($stringValue)
  {
    $this->stringValue = $stringValue;
  }
  /**
   * @return string
   */
  public function getStringValue()
  {
    return $this->stringValue;
  }
  /**
   * @param GoogleCloudAiplatformV1StructValue
   */
  public function setStructValue(GoogleCloudAiplatformV1StructValue $structValue)
  {
    $this->structValue = $structValue;
  }
  /**
   * @return GoogleCloudAiplatformV1StructValue
   */
  public function getStructValue()
  {
    return $this->structValue;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1FeatureValue::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1FeatureValue');
