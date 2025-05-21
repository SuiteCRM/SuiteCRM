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

class CheckSetResult extends \Google\Model
{
  protected $allowlistResultType = AllowlistResult::class;
  protected $allowlistResultDataType = '';
  protected $checkResultsType = CheckResults::class;
  protected $checkResultsDataType = '';
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $explanation;
  /**
   * @var string
   */
  public $index;
  protected $scopeType = Scope::class;
  protected $scopeDataType = '';

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
   * @param CheckResults
   */
  public function setCheckResults(CheckResults $checkResults)
  {
    $this->checkResults = $checkResults;
  }
  /**
   * @return CheckResults
   */
  public function getCheckResults()
  {
    return $this->checkResults;
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
   * @param Scope
   */
  public function setScope(Scope $scope)
  {
    $this->scope = $scope;
  }
  /**
   * @return Scope
   */
  public function getScope()
  {
    return $this->scope;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CheckSetResult::class, 'Google_Service_BinaryAuthorization_CheckSetResult');
