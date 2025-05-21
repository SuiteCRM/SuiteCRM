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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1DataProfileResultProfileFieldProfileInfoDoubleFieldInfo extends \Google\Collection
{
  protected $collection_key = 'quartiles';
  public $average;
  public $max;
  public $min;
  public $quartiles;
  public $standardDeviation;

  public function setAverage($average)
  {
    $this->average = $average;
  }
  public function getAverage()
  {
    return $this->average;
  }
  public function setMax($max)
  {
    $this->max = $max;
  }
  public function getMax()
  {
    return $this->max;
  }
  public function setMin($min)
  {
    $this->min = $min;
  }
  public function getMin()
  {
    return $this->min;
  }
  public function setQuartiles($quartiles)
  {
    $this->quartiles = $quartiles;
  }
  public function getQuartiles()
  {
    return $this->quartiles;
  }
  public function setStandardDeviation($standardDeviation)
  {
    $this->standardDeviation = $standardDeviation;
  }
  public function getStandardDeviation()
  {
    return $this->standardDeviation;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataProfileResultProfileFieldProfileInfoDoubleFieldInfo::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataProfileResultProfileFieldProfileInfoDoubleFieldInfo');
