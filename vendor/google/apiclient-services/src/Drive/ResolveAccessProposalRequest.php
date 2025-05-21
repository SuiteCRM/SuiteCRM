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

namespace Google\Service\Drive;

class ResolveAccessProposalRequest extends \Google\Collection
{
  protected $collection_key = 'role';
  /**
   * @var string
   */
  public $action;
  /**
   * @var string[]
   */
  public $role;
  /**
   * @var bool
   */
  public $sendNotification;
  /**
   * @var string
   */
  public $view;

  /**
   * @param string
   */
  public function setAction($action)
  {
    $this->action = $action;
  }
  /**
   * @return string
   */
  public function getAction()
  {
    return $this->action;
  }
  /**
   * @param string[]
   */
  public function setRole($role)
  {
    $this->role = $role;
  }
  /**
   * @return string[]
   */
  public function getRole()
  {
    return $this->role;
  }
  /**
   * @param bool
   */
  public function setSendNotification($sendNotification)
  {
    $this->sendNotification = $sendNotification;
  }
  /**
   * @return bool
   */
  public function getSendNotification()
  {
    return $this->sendNotification;
  }
  /**
   * @param string
   */
  public function setView($view)
  {
    $this->view = $view;
  }
  /**
   * @return string
   */
  public function getView()
  {
    return $this->view;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ResolveAccessProposalRequest::class, 'Google_Service_Drive_ResolveAccessProposalRequest');
