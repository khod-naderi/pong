VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   11520
   ClientLeft      =   0
   ClientTop       =   -135
   ClientWidth     =   15360
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   MouseIcon       =   "Form1.frx":23C0D
   MousePointer    =   99  'Custom
   ScaleHeight     =   768
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   1024
   ShowInTaskbar   =   0   'False
   WindowState     =   2  'Maximized
   Begin VB.Timer Timer3 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   2880
      Top             =   10680
   End
   Begin VB.Timer Timer2 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   3840
      Top             =   10680
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   3360
      Top             =   10680
   End
   Begin VB.Label Label6 
      BackStyle       =   0  'Transparent
      Caption         =   "KeyPress Enter For Start/Stop  KeyPress Alt for Now"
      ForeColor       =   &H000000FF&
      Height          =   255
      Left            =   10680
      TabIndex        =   5
      Top             =   11040
      Width           =   3975
   End
   Begin VB.Label Label5 
      BackStyle       =   0  'Transparent
      Caption         =   "KeyPress 1 For One Player KeyPress 2 For Two Player"
      ForeColor       =   &H000000FF&
      Height          =   255
      Left            =   10680
      TabIndex        =   4
      Top             =   10800
      Width           =   3975
   End
   Begin VB.Label Label4 
      BackStyle       =   0  'Transparent
      Caption         =   "KeyPress ESC For Exit"
      ForeColor       =   &H000000FF&
      Height          =   255
      Left            =   6840
      TabIndex        =   3
      Top             =   11040
      Width           =   1695
   End
   Begin VB.Shape Shape2 
      BackColor       =   &H00C0C0FF&
      BorderColor     =   &H00FFFFFF&
      Height          =   2175
      Left            =   6600
      Shape           =   3  'Circle
      Top             =   3840
      Width           =   2055
   End
   Begin VB.Shape Shape1 
      BorderColor     =   &H00FFFFFF&
      FillColor       =   &H00C0C0C0&
      Height          =   10455
      Left            =   0
      Top             =   0
      Width           =   15330
   End
   Begin VB.Label Label3 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   24
         Charset         =   178
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   615
      Left            =   9000
      TabIndex        =   2
      Top             =   10560
      Width           =   1095
   End
   Begin VB.Label Label2 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   24
         Charset         =   178
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   615
      Left            =   5760
      TabIndex        =   1
      Top             =   10560
      Width           =   1095
   End
   Begin VB.Image Image3 
      Height          =   495
      Left            =   7350
      Picture         =   "Form1.frx":45921
      Stretch         =   -1  'True
      Top             =   4650
      Width           =   495
   End
   Begin VB.Image Image2 
      Height          =   2415
      Left            =   60
      Picture         =   "Form1.frx":69095
      Stretch         =   -1  'True
      Top             =   3750
      Width           =   735
   End
   Begin VB.Image Image1 
      Height          =   2415
      Left            =   14460
      Picture         =   "Form1.frx":8D999
      Stretch         =   -1  'True
      Top             =   3750
      Width           =   735
   End
   Begin VB.Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "Game TTGO Im Ali naderi"
      ForeColor       =   &H000000FF&
      Height          =   615
      Left            =   7200
      TabIndex        =   0
      Top             =   10560
      Width           =   975
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim R, R1, R2, R3, O, P, S As Integer, Y12, Y22 As Long

Private Sub Form_KeyDown(KC As Integer, Shift As Integer)
If KC = 27 Then End
If KC = 49 Then
P = 1
Label2 = 0
Label3 = 0
Image3.Left = 490
Image3.Top = 310
End If
If KC = 50 Then
P = 2
Label2 = 0
Label3 = 0
Image3.Left = 490
Image3.Top = 310
End If
If KC = 38 Then R1 = 1
If KC = 40 Then R1 = 2
If KC = 87 Then R3 = 1
If KC = 83 Then R3 = 2
If KC = 18 Then
Timer1.Enabled = False
Timer2.Enabled = False
Timer3.Enabled = False
Image3.Left = 490
Image3.Top = 310
Label2 = 0
Label3 = 0
Image2.Top = 250
Image1.Top = 250
Image1.Left = 964
Image2.Left = 4
End If
If KC = 13 And S = 0 Then
Timer1.Enabled = True
Timer2.Enabled = True
Timer3.Enabled = True
S = 1
ElseIf KC = 13 And S = 1 Then
Timer1.Enabled = False
Timer2.Enabled = False
Timer3.Enabled = False
S = 0
End If
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
If KeyCode = 38 Then R1 = 0
If KeyCode = 40 Then R1 = 0
If KeyCode = 87 Then R3 = 0
If KeyCode = 83 Then R3 = 0
End Sub

Private Sub Form_Load()
R = 1
P = 1
R2 = Int(Rnd * (4) + 1)
End Sub

Private Sub Timer1_Timer()
If R2 = 1 Then
Image3.Top = Image3.Top + 5
Image3.Left = Image3.Left + 5
End If
If R2 = 2 Then
Image3.Top = Image3.Top - 5
Image3.Left = Image3.Left - 5
End If
If R2 = 3 Then
Image3.Top = Image3.Top - 5
Image3.Left = Image3.Left + 5
End If
If R2 = 4 Then
Image3.Top = Image3.Top + 5
Image3.Left = Image3.Left - 5
End If
If Image3.Top <= 0 Then
Image3.Top = 10
If R2 = 3 Then R2 = 1
If R2 = 2 Then R2 = 4
End If
If Image3.Top >= 650 Then
Image3.Top = 640
If R2 = 4 Then R2 = 2
If R2 = 1 Then R2 = 3
End If
If Image3.Left <= 10 Then
Image3.Left = 490
Image3.Top = 310
Label3 = Label3 + 1
R2 = Int(Rnd * (4) + 1)
End If
If Image3.Left >= 970 Then
Image3.Left = 490
Image3.Top = 310
Label2 = Label2 + 1
R2 = Int(Rnd * (4) + 1)
End If
If Image3.Top > Image2.Top And Image3.Top < Y12 And Image3.Left <= 64 Then
O = Int(Rnd * (4) + 1)
If O > 2 Then R2 = 3
If O <= 2 Then R2 = 1
End If
If Image3.Top > Image1.Top And Image3.Top < Y22 And Image3.Left >= 940 Then
O = Int(Rnd * (4) + 1)
If O > 2 Then R2 = 2
If O <= 2 Then R2 = 4
End If
End Sub

Private Sub Timer2_Timer()
Y22 = Image1.Top + Image1.Height
If P = 1 Then
If R = 1 Then Image1.Top = Image1.Top + 10
If R = 2 Then Image1.Top = Image1.Top - 10
If Image1.Top = 530 Then R = 2
If Image1.Top = 0 Then R = 1
If Image1.Top >= 540 Then Image1.Top = 520
If Image1.Top <= -10 Then Image1.Top = 10
ElseIf P = 2 Then
If R3 = 1 Then Image1.Top = Image1.Top - 10
If R3 = 2 Then Image1.Top = Image1.Top + 10
If Image1.Top >= 530 Then R3 = 0
If Image1.Top <= 0 Then R3 = 0
If Image1.Top >= 540 Then Image1.Top = 530
If Image1.Top <= -10 Then Image1.Top = 0
End If
End Sub

Private Sub Timer3_Timer()
Y12 = Image2.Top + Image2.Height
If R1 = 1 Then Image2.Top = Image2.Top - 10
If R1 = 2 Then Image2.Top = Image2.Top + 10
If Image2.Top >= 530 Then R1 = 0
If Image2.Top <= 0 Then R1 = 0
If Image2.Top >= 540 Then Image2.Top = 530
If Image2.Top <= -10 Then Image2.Top = 0
End Sub
