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

class GoogleCloudAiplatformV1NearestNeighborQueryNumericFilter extends \Google\Model
{
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $op;
  public $valueDouble;
  /**
   * @var float
   */
  public $valueFloat;
  /**
   * @var string
   */
  public $valueInt;

  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setOp($op)
  {
    $this->op = $op;
  }
  /**
   * @return string
   */
  public function getOp()
  {
    return $this->op;
  }
  public function setValueDouble($valueDouble)
  {
    $this->valueDouble = $valueDouble;
  }
  public function getValueDouble()
  {
    return $this->valueDouble;
  }
  /**
   * @param float
   */
  public function setValueFloat($valueFloat)
  {
    $this->valueFloat = $valueFloat;
  }
  /**
   * @return float
   */
  public function getValueFloat()
  {
    return $this->valueFloat;
  }
  /**
   * @param string
   */
  public function setValueInt($valueInt)
  {
    $this->valueInt = $valueInt;
  }
  /**
   * @return string
   */
  public function getValueInt()
  {
    return $this->valueInt;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1NearestNeighborQueryNumericFilter::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1NearestNeighborQueryNumericFilter');
