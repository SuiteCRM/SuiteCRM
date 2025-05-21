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

class GoogleCloudAiplatformV1TimeSeriesDataPoint extends \Google\Model
{
  protected $blobsType = GoogleCloudAiplatformV1TensorboardBlobSequence::class;
  protected $blobsDataType = '';
  protected $scalarType = GoogleCloudAiplatformV1Scalar::class;
  protected $scalarDataType = '';
  /**
   * @var string
   */
  public $step;
  protected $tensorType = GoogleCloudAiplatformV1TensorboardTensor::class;
  protected $tensorDataType = '';
  /**
   * @var string
   */
  public $wallTime;

  /**
   * @param GoogleCloudAiplatformV1TensorboardBlobSequence
   */
  public function setBlobs(GoogleCloudAiplatformV1TensorboardBlobSequence $blobs)
  {
    $this->blobs = $blobs;
  }
  /**
   * @return GoogleCloudAiplatformV1TensorboardBlobSequence
   */
  public function getBlobs()
  {
    return $this->blobs;
  }
  /**
   * @param GoogleCloudAiplatformV1Scalar
   */
  public function setScalar(GoogleCloudAiplatformV1Scalar $scalar)
  {
    $this->scalar = $scalar;
  }
  /**
   * @return GoogleCloudAiplatformV1Scalar
   */
  public function getScalar()
  {
    return $this->scalar;
  }
  /**
   * @param string
   */
  public function setStep($step)
  {
    $this->step = $step;
  }
  /**
   * @return string
   */
  public function getStep()
  {
    return $this->step;
  }
  /**
   * @param GoogleCloudAiplatformV1TensorboardTensor
   */
  public function setTensor(GoogleCloudAiplatformV1TensorboardTensor $tensor)
  {
    $this->tensor = $tensor;
  }
  /**
   * @return GoogleCloudAiplatformV1TensorboardTensor
   */
  public function getTensor()
  {
    return $this->tensor;
  }
  /**
   * @param string
   */
  public function setWallTime($wallTime)
  {
    $this->wallTime = $wallTime;
  }
  /**
   * @return string
   */
  public function getWallTime()
  {
    return $this->wallTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1TimeSeriesDataPoint::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1TimeSeriesDataPoint');
