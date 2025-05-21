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

namespace Google\Service\SecurityCommandCenter;

class GoogleCloudSecuritycenterV2Binding extends \Google\Collection
{
  protected $collection_key = 'subjects';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $ns;
  protected $roleType = GoogleCloudSecuritycenterV2Role::class;
  protected $roleDataType = '';
  protected $subjectsType = GoogleCloudSecuritycenterV2Subject::class;
  protected $subjectsDataType = 'array';

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
  public function setNs($ns)
  {
    $this->ns = $ns;
  }
  /**
   * @return string
   */
  public function getNs()
  {
    return $this->ns;
  }
  /**
   * @param GoogleCloudSecuritycenterV2Role
   */
  public function setRole(GoogleCloudSecuritycenterV2Role $role)
  {
    $this->role = $role;
  }
  /**
   * @return GoogleCloudSecuritycenterV2Role
   */
  public function getRole()
  {
    return $this->role;
  }
  /**
   * @param GoogleCloudSecuritycenterV2Subject[]
   */
  public function setSubjects($subjects)
  {
    $this->subjects = $subjects;
  }
  /**
   * @return GoogleCloudSecuritycenterV2Subject[]
   */
  public function getSubjects()
  {
    return $this->subjects;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2Binding::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2Binding');
