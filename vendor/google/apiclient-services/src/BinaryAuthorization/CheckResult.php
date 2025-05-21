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

namespace Google\Service\BinaryAuthorization;

class CheckResult extends \Google\Model
{
  protected $allowlistResultType = AllowlistResult::class;
  protected $allowlistResultDataType = '';
  /**
   * @var string
   */
  public $displayName;
  protected $evaluationResultType = EvaluationResult::class;
  protected $evaluationResultDataType = '';
  /**
   * @var string
   */
  public $explanation;
  /**
   * @var string
   */
  public $index;
  /**
   * @var string
   */
  public $type;

  /**
   * @param AllowlistResult
   */
  public function setAllowlistResult(AllowlistResult $allowlistResult)
  {
    $this->allowlistResult = $allowlistResult;
  }
  /**
   * @return AllowlistResult
   */
  public function getAllowlistResult()
  {
    return $this->allowlistResult;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param EvaluationResult
   */
  public function setEvaluationResult(EvaluationResult $evaluationResult)
  {
    $this->evaluationResult = $evaluationResult;
  }
  /**
   * @return EvaluationResult
   */
  public function getEvaluationResult()
  {
    return $this->evaluationResult;
  }
  /**
   * @param string
   */
  public function setExplanation($explanation)
  {
    $this->explanation = $explanation;
  }
  /**
   * @return string
   */
  public function getExplanation()
  {
    return $this->explanation;
  }
  /**
   * @param string
   */
  public function setIndex($index)
  {
    $this->index = $index;
  }
  /**
   * @return string
   */
  public function getIndex()
  {
    return $this->index;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CheckResult::class, 'Google_Service_BinaryAuthorization_CheckResult');
