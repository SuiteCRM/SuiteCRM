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

namespace Google\Service\BigQueryDataPolicyService;

class DataMaskingPolicy extends \Google\Model
{
  /**
   * @var string
   */
  public $predefinedExpression;
  /**
   * @var string
   */
  public $routine;

  /**
   * @param string
   */
  public function setPredefinedExpression($predefinedExpression)
  {
    $this->predefinedExpression = $predefinedExpression;
  }
  /**
   * @return string
   */
  public function getPredefinedExpression()
  {
    return $this->predefinedExpression;
  }
  /**
   * @param string
   */
  public function setRoutine($routine)
  {
    $this->routine = $routine;
  }
  /**
   * @return string
   */
  public function getRoutine()
  {
    return $this->routine;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DataMaskingPolicy::class, 'Google_Service_BigQueryDataPolicyService_DataMaskingPolicy');
