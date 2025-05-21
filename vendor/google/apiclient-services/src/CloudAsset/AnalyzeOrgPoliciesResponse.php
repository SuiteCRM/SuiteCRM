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

namespace Google\Service\CloudAsset;

class AnalyzeOrgPoliciesResponse extends \Google\Collection
{
  protected $collection_key = 'orgPolicyResults';
  protected $constraintType = AnalyzerOrgPolicyConstraint::class;
  protected $constraintDataType = '';
  /**
   * @var string
   */
  public $nextPageToken;
  protected $orgPolicyResultsType = OrgPolicyResult::class;
  protected $orgPolicyResultsDataType = 'array';

  /**
   * @param AnalyzerOrgPolicyConstraint
   */
  public function setConstraint(AnalyzerOrgPolicyConstraint $constraint)
  {
    $this->constraint = $constraint;
  }
  /**
   * @return AnalyzerOrgPolicyConstraint
   */
  public function getConstraint()
  {
    return $this->constraint;
  }
  /**
   * @param string
   */
  public function setNextPageToken($nextPageToken)
  {
    $this->nextPageToken = $nextPageToken;
  }
  /**
   * @return string
   */
  public function getNextPageToken()
  {
    return $this->nextPageToken;
  }
  /**
   * @param OrgPolicyResult[]
   */
  public function setOrgPolicyResults($orgPolicyResults)
  {
    $this->orgPolicyResults = $orgPolicyResults;
  }
  /**
   * @return OrgPolicyResult[]
   */
  public function getOrgPolicyResults()
  {
    return $this->orgPolicyResults;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AnalyzeOrgPoliciesResponse::class, 'Google_Service_CloudAsset_AnalyzeOrgPoliciesResponse');
