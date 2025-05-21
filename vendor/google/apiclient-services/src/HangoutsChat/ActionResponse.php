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

namespace Google\Service\HangoutsChat;

class ActionResponse extends \Google\Model
{
  protected $dialogActionType = DialogAction::class;
  protected $dialogActionDataType = '';
  /**
   * @var string
   */
  public $type;
  protected $updatedWidgetType = UpdatedWidget::class;
  protected $updatedWidgetDataType = '';
  /**
   * @var string
   */
  public $url;

  /**
   * @param DialogAction
   */
  public function setDialogAction(DialogAction $dialogAction)
  {
    $this->dialogAction = $dialogAction;
  }
  /**
   * @return DialogAction
   */
  public function getDialogAction()
  {
    return $this->dialogAction;
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
  /**
   * @param UpdatedWidget
   */
  public function setUpdatedWidget(UpdatedWidget $updatedWidget)
  {
    $this->updatedWidget = $updatedWidget;
  }
  /**
   * @return UpdatedWidget
   */
  public function getUpdatedWidget()
  {
    return $this->updatedWidget;
  }
  /**
   * @param string
   */
  public function setUrl($url)
  {
    $this->url = $url;
  }
  /**
   * @return string
   */
  public function getUrl()
  {
    return $this->url;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ActionResponse::class, 'Google_Service_HangoutsChat_ActionResponse');
