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

class GoogleCloudAiplatformV1Tensor extends \Google\Collection
{
  protected $collection_key = 'uintVal';
  /**
   * @var bool[]
   */
  public $boolVal;
  /**
   * @var string[]
   */
  public $bytesVal;
  public $doubleVal;
  /**
   * @var string
   */
  public $dtype;
  /**
   * @var float[]
   */
  public $floatVal;
  /**
   * @var string[]
   */
  public $int64Val;
  /**
   * @var int[]
   */
  public $intVal;
  protected $listValType = GoogleCloudAiplatformV1Tensor::class;
  protected $listValDataType = 'array';
  /**
   * @var string[]
   */
  public $shape;
  /**
   * @var string[]
   */
  public $stringVal;
  protected $structValType = GoogleCloudAiplatformV1Tensor::class;
  protected $structValDataType = 'map';
  /**
   * @var string
   */
  public $tensorVal;
  /**
   * @var string[]
   */
  public $uint64Val;
  /**
   * @var string[]
   */
  public $uintVal;

  /**
   * @param bool[]
   */
  public function setBoolVal($boolVal)
  {
    $this->boolVal = $boolVal;
  }
  /**
   * @return bool[]
   */
  public function getBoolVal()
  {
    return $this->boolVal;
  }
  /**
   * @param string[]
   */
  public function setBytesVal($bytesVal)
  {
    $this->bytesVal = $bytesVal;
  }
  /**
   * @return string[]
   */
  public function getBytesVal()
  {
    return $this->bytesVal;
  }
  public function setDoubleVal($doubleVal)
  {
    $this->doubleVal = $doubleVal;
  }
  public function getDoubleVal()
  {
    return $this->doubleVal;
  }
  /**
   * @param string
   */
  public function setDtype($dtype)
  {
    $this->dtype = $dtype;
  }
  /**
   * @return string
   */
  public function getDtype()
  {
    return $this->dtype;
  }
  /**
   * @param float[]
   */
  public function setFloatVal($floatVal)
  {
    $this->floatVal = $floatVal;
  }
  /**
   * @return float[]
   */
  public function getFloatVal()
  {
    return $this->floatVal;
  }
  /**
   * @param string[]
   */
  public function setInt64Val($int64Val)
  {
    $this->int64Val = $int64Val;
  }
  /**
   * @return string[]
   */
  public function getInt64Val()
  {
    return $this->int64Val;
  }
  /**
   * @param int[]
   */
  public function setIntVal($intVal)
  {
    $this->intVal = $intVal;
  }
  /**
   * @return int[]
   */
  public function getIntVal()
  {
    return $this->intVal;
  }
  /**
   * @param GoogleCloudAiplatformV1Tensor[]
   */
  public function setListVal($listVal)
  {
    $this->listVal = $listVal;
  }
  /**
   * @return GoogleCloudAiplatformV1Tensor[]
   */
  public function getListVal()
  {
    return $this->listVal;
  }
  /**
   * @param string[]
   */
  public function setShape($shape)
  {
    $this->shape = $shape;
  }
  /**
   * @return string[]
   */
  public function getShape()
  {
    return $this->shape;
  }
  /**
   * @param string[]
   */
  public function setStringVal($stringVal)
  {
    $this->stringVal = $stringVal;
  }
  /**
   * @return string[]
   */
  public function getStringVal()
  {
    return $this->stringVal;
  }
  /**
   * @param GoogleCloudAiplatformV1Tensor[]
   */
  public function setStructVal($structVal)
  {
    $this->structVal = $structVal;
  }
  /**
   * @return GoogleCloudAiplatformV1Tensor[]
   */
  public function getStructVal()
  {
    return $this->structVal;
  }
  /**
   * @param string
   */
  public function setTensorVal($tensorVal)
  {
    $this->tensorVal = $tensorVal;
  }
  /**
   * @return string
   */
  public function getTensorVal()
  {
    return $this->tensorVal;
  }
  /**
   * @param string[]
   */
  public function setUint64Val($uint64Val)
  {
    $this->uint64Val = $uint64Val;
  }
  /**
   * @return string[]
   */
  public function getUint64Val()
  {
    return $this->uint64Val;
  }
  /**
   * @param string[]
   */
  public function setUintVal($uintVal)
  {
    $this->uintVal = $uintVal;
  }
  /**
   * @return string[]
   */
  public function getUintVal()
  {
    return $this->uintVal;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1Tensor::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1Tensor');
